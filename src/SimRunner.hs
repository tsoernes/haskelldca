{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimRunner where

import           AccUtils
import           Agent ( backward )
import           Base
import           Control.Lens
import           Control.Monad.Extra (loopM, whenJust)
import           Control.Monad.Reader ( MonadReader, asks, runReader , ReaderT, ask)
import           Control.Monad.State
    ( Monad(return),
      StateT(runStateT),
      MonadState,
      evalState,
      when
      )
import           Data.Array.Accelerate (Acc, Scalar, the)
import qualified Data.Array.Accelerate as A
import           Data.Either
import           Data.Time.Clock ( diffUTCTime, getCurrentTime )
import           Data.Word ( Word64 )
import           Gridfuncs (violatesReuseConstraint, featureRep, featureRep' )
import           LensUtils ( statePartM )
import           Opt
import           Prelude as P
import           Simulator
import           Stats
import           Text.Printf ( printf )
import           Debug.Trace ( trace )
import           Control.Exception (assert)

data SimError = NEligOverflow
              | ReuseConstraintViolated deriving (Show, Eq)

-- The different reasons why the simulation has stopped (in the case of 'Paused' only temporarily)
data SimStop = Success
             | Paused
             | ZeroLoss Float
             | NaNLoss
             | InternalError SimError
             | UserQuit deriving (Show, Eq)


-- | The inner loop which executes a single step in the simulator. First,
-- | the agent selects an action which is executed on the network grid. In
-- | return, the caller environment emits the next event for which an action
-- | must be selected, and a reward.
-- | Based on the reward, the agent adjusts its evaluation of the grid state
-- | prior to action.
-- | Return training loss if not NaN.
runStep :: (MonadReader Opt m, MonadState SimState m)
  => (Scalar Cell -> Scalar Bool -> Agent -> Grid -> Frep -> (Scalar (Maybe Ch), Frep))
  -> (Scalar Float -> Scalar Float -> Scalar Float -> Scalar Bool -> Scalar Cell -> Scalar (Maybe Ch) -> Frep -> Frep -> Grid -> Agent -> (Grid, Agent, Scalar (Maybe Float), Scalar Int))
  -> m (Maybe Float)
runStep accFn1 accFn2 = do
  -- Pull out all arrays from state
  bkend <- asks backend
  agent <- use ssAgent
  grid <- use ssGrid
  frep <- use ssFrep
  event <- use ssEvent

  -- Select an action based on current grid and event
  let cell = scalar (event^.evCell)
      eIsEnd = scalar $ isEnd (event^.evType)
      (mbCh, nextFrep) = accFn1 cell eIsEnd agent grid frep

  -- Generate next events; receive the next event to be processed.
  environmentStep (theR mbCh)

  alphaN <- asks (scalar . alphaNet)
  alphaA <- asks (scalar . alphaAvg)
  alphaG <- asks (scalar . alphaGrad)
  let -- Execute action on the grid, receive a reward; then train the agent on
      -- the just observed state transition and reward.
      (nextGrid, nextAgent, loss, reward) = accFn2 alphaN alphaA alphaG eIsEnd cell mbCh frep nextFrep grid agent
  -- Update the rest of the state
  ssAgent .= nextAgent
  ssGrid .= nextGrid
  ssFrep .= nextFrep

  ssPGrid .= grid
  ssPFrep .= frep
  ssPAgent .= agent
  ssPEvent .= event

  let ret = theR loss
      nextFrep' = runN bkend featureRep' nextGrid
      ret1 = assert (nextFrep == nextFrep') ret
      -- ret3 = trace (printf "Reward|Loss: %d %s" (theR reward) (show ret)) ret2
  return ret1


gridStepTrain :: Acc (Scalar Float)
  -> Acc (Scalar Float)
  -> Acc (Scalar Float)
  -> Acc (Scalar Bool)
  -> Acc (Scalar Cell)
  -> Acc (Scalar (Maybe Ch))
  -> Acc Frep
  -> Acc Frep
  -> Acc Grid
  -> Acc Agent
  -> Acc (Grid, Agent, Scalar (Maybe Float), Scalar Int)
gridStepTrain alphaN alphaA alphaG eIsEnd eCell mbCh frep nextFrep grid agent =
  A.lift (grid', agent', loss, reward)
  where
    A2 reward grid' = gridStep (the eIsEnd) (the eCell) (the mbCh) grid
    -- Train agent on the just received experience consisting of
    -- the state transition (in feature space) and a reward.
    reward' = A.fromIntegral $ the reward
    A2 loss agent' = backward (the alphaN) (the alphaA) (the alphaG) frep nextFrep reward' agent


untilJust :: (Monad m) => m (a, Maybe b) -> m ([a], b)
untilJust f = go
    where go = do
            x <- f
            case snd x of
              Nothing -> do
                (xs, b) <- go
                return (fst x : xs, b)
              Just b -> return ([fst x], b)


runPeriod :: forall m. (MonadReader Opt m, MonadState SimState m)
  => m (Maybe Float) -- runStep, partially applied with the Acc functions
  -> m (String, Maybe SimStop)
runPeriod accRunStep = do
  lgIter <- asks logIter
  nEvents' <- asks nEvents
  bkend <- asks backend
  minLoss' <- asks minLoss
  verifyRC <- asks verifyReuseConstraint
  iStart <- use ssIter
  let runStep' :: (MonadReader Opt m, MonadState SimState m)
        => m (Float, Maybe SimStop)
      runStep' = do
        mbLoss <- accRunStep
        grid <- use ssGrid
        frep <- use ssFrep
        iTotal <- use ssIter
        let
            maxNElig = theR $ runN bkend (A.maximum . A.flatten) frep
        return $ case mbLoss of
          Nothing -> (-1, Just NaNLoss)
          Just loss
            -- There's a bug in the program; break out.
            -- TODO instead dump all of acc state + event + iter + agent.
            -- TODO pre and on overflow
            | verifyRC && runExp bkend (violatesReuseConstraint (A.use grid))
              -> (loss, Just $ InternalError ReuseConstraintViolated)
            -- The count for eligible channels is too high. Bug in program.
            | maxNElig > 70
              -> (loss, Just $ InternalError NEligOverflow)
            -- Agent has finished training (likely it has not trained at all)
            | minLoss' > 0 && iStart > 50 && abs loss < minLoss'
              -> (loss, Just (ZeroLoss loss))
            -- Simulation finished
            | iTotal == nEvents'
              -> (loss, Just Success)
            -- Period finished; print some stats and what not
            | iTotal-iStart == lgIter
              -> (loss, Just Paused)
            | otherwise
              -> (loss, Nothing)
  (losses, simstop) <- untilJust runStep'
  -- Get statistics report and reset period counters
  i <- use ssIter
  avgR <- uses (ssAgent . _3) theR
  report <- statePartM ssStats (statsReportPeriod i losses avgR)
  return $ case simstop of
    Paused -> (report, Nothing)
    _ -> (report, Just simstop)


-- | Run 'logIter' steps in the simulator then
-- | print some statistics detailing the just-finished period.
runPeriodWrapper :: StateT SimState (ReaderT Opt Identity) (String, Maybe SimStop)
  -> Opt
  -> SimState
  -> IO (Either SimState (SimState, SimStop))
runPeriodWrapper accRunLogIter opts currentState = do
  let (liRes, newState) = runReader (runStateT accRunLogIter currentState) opts
  putStrLn $ fst liRes
  whenJust (snd liRes) print
  return $ case snd liRes of
    Nothing -> Left newState
    Just ss -> Right (newState, ss)


-- | Given random seed and simulation options, run the simulation
-- | and print out stats.
runSim :: Word64 -> Opt -> IO ()
runSim seed opts = do
  let bkend = backend opts
      accFn1 = runN bkend getAction
      accFn2 = runN bkend gridStepTrain
  -- Execute 'nEvents' in the simulator,
  -- or until a non-pause 'simstop' occurs.
  -- TODO graceful quit on CTRL-C
  startTime <- getCurrentTime
  let sState = runReader (mkSimState seed) opts
      runPeriodWrapperAcc = runPeriodWrapper $ runPeriod $ runStep accFn1 accFn2
      fn = runPeriodWrapperAcc opts
  (sState', simstop) <- loopM fn sState

  -- Print out useful info in case of bugs
  case simstop of
   InternalError ie -> do
     let prevFrep = sState'^.ssPFrep
         curFrep = sState'^.ssFrep
         runMax arr = theR $ runN bkend (A.maximum . A.flatten) arr
         prevMaxNElig = runMax prevFrep
         curMaxNElig = runMax curFrep
     print "Prev event"
     print  $ sState'^.ssPEvent
     print "Prev grid"
     putStrLn . showGrid $ sState'^.ssPGrid
     print $ "Prev frep | max: " ++ show prevMaxNElig
     print $ sState'^.ssPFrep
     print "Current iter"
     print $ sState'^.ssIter
     print "Current event"
     print $ sState'^.ssEvent
     print "Current grid"
     putStrLn . showGrid $ sState'^.ssGrid
     print $ "Current frep | max: " ++ show curMaxNElig
     print curFrep
     case ie of
       ReuseConstraintViolated ->
         print $ "Channels in use:" ++
         (show . runN bkend $ indicesOf3 $ A.use (sState'^.ssGrid))
       NEligOverflow -> do
         let prevMaxWnet = runMax (sState'^.ssPAgent._1)
             curMaxWnet = runMax (sState'^.ssAgent._1)
             prevMaxWgrad = runMax (sState'^.ssPAgent._2)
             curMaxWgrad = runMax (sState'^.ssAgent._2)
         putStrLn $ printf "Prev agent | max wnet: %.2f | max wgrad: %.2f" prevMaxWnet prevMaxWgrad
         print $ sState'^.ssPAgent
         putStrLn $ printf "Current agent | max wnet: %.2f | max wgrad: %.2f" curMaxWnet curMaxWgrad
         print $ sState'^.ssAgent
   _ -> return ()

  -- Print the final 'statistics' report
  let nCallsInProgress = runExp bkend $ boolSum (A.use (sState'^.ssGrid))
      i = view ssIter sState'
  putStrLn $ evalState (statsReportEnd nCallsInProgress i) (sState'^.ssStats)

  -- Print simulation duration and speed (in wall-clock time)
  endTime <- getCurrentTime -- in seconds
  let dt_sec_double = fromRational $ toRational $ diffUTCTime endTime startTime :: Double
      dt_sec = floor dt_sec_double :: Int
      dt_min = floor (dt_sec_double / 60.0) :: Int
      dt_rem_sec = dt_sec - dt_min * 60 :: Int
      rate = fromIntegral i / dt_sec_double :: Double
      last_etime = sState' ^. ssEvent . evTime :: Double -- in minutes
      sim_dt_min = floor last_etime :: Int
      sim_dt_rem_sec = floor ((last_etime - fromIntegral sim_dt_min) * 60.0) :: Int
    in putStrLn $ printf
        "\nSimulation duration: %dm%ds in sim time,\
        \ %dm%ds wall clock with speed %d events at %.0f events/second"
        sim_dt_min sim_dt_rem_sec
        dt_min dt_rem_sec
        i rate
