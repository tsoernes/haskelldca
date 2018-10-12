{-# LANGUAGE FlexibleContexts #-}

module SimRunner where

import           AccUtils
import           Agent ( backward )
import           Base
import           Control.Lens
import           Control.Monad.Reader ( MonadReader, asks, runReader )
import           Control.Monad.Extra (loopM, whenJust)
import Data.Either
import           Control.Monad.State
    ( Monad(return),
      StateT(runStateT),
      MonadState,
      evalState,
      )
import Data.Array.Accelerate (Acc, Scalar, Exp, the)
import qualified Data.Array.Accelerate as A
import           Data.Time.Clock ( diffUTCTime, getCurrentTime, NominalDiffTime )
import           Data.Word ( Word64 )
import Gridfuncs (violatesReuseConstraint )
import           LensUtils ( statePartM )
import           Opt
import           Prelude as P
import           Simulator
import           Stats
import           Text.Printf ( printf )
import           Debug.Trace ( trace )


-- | The inner loop which executes a single step in the simulator. First,
-- | the agent selects an action which is executed on the network grid. In
-- | return, the caller environment emits the next event for which an action
-- | must be selected, and a reward.
-- | Based on the reward, the agent adjusts its evaluation of the grid state
-- | prior to action. The so-called value function yields a
-- | numeric evaluation of an arbitrary grid state which can be thought
-- | of as the degree.
-- | Return training loss, if not NaN.
runStep :: (MonadReader Opt m, MonadState SimState m) => m (Maybe Float)
runStep = do
  -- This is the only place in the program where Acc expressions are executed.
  bkend <- asks backend
  -- Pull out all arrays from state, and put them into Acc
  agent <- use ssAgent
  grid <- use ssGrid
  frep <- use ssFrep
  event <- use ssEvent

  -- Select an action based on current grid and event
  let cell = scalar (event^.evCell)
      eIsEnd = scalar $ isEnd (event^.evType)
      runner = runN bkend getAction'
      (mbCh, frep') = runner cell eIsEnd agent grid frep

  -- Execute the action in the environment (update grid; receive next event) and receive a reward.
  environmentStep (theR mbCh)

  alphaN <- asks (scalar . alphaNet)
  alphaG <- asks (scalar . alphaGrad)
  let runner2 = runN bkend runAcc
      accRes = runner2 alphaN alphaG eIsEnd cell mbCh frep frep' grid agent
      (grid', agent', loss) = accRes
  -- Put the results back into the state
  ssFrep .= frep'
  ssAgent .= agent'
  ssGrid .= grid'
  return $ theR loss


runAcc :: Acc (Scalar Float)
  -> Acc (Scalar Float)
  -> Acc (Scalar Bool)
  -> Acc (Scalar Cell)
  -> Acc (Scalar (Maybe Ch))
  -> Acc Frep
  -> Acc Frep
  -> Acc Grid
  -> Acc Agent
  -> Acc (Grid, Agent, Scalar (Maybe Float))
runAcc alphaN alphaG eIsEnd eCell mbCh frep nextFrep grid agent = A.lift (grid', agent', loss)
  where
    A2 reward grid' = gridStep (the eIsEnd) (the eCell) (the mbCh) grid
    -- Train agent on the just received experience consisting of
    -- the state transition (in feature space) and a reward.
    reward' = the $ A.map A.fromIntegral reward
    A2 loss agent' = backward (the alphaN) (the alphaG) frep nextFrep reward' agent

data SimStop = Success | Paused | NaNLoss | NoLoss Float | ReuseConstraintViolated | UserQuit deriving (Show)

untilJust :: (Monad m) => m (a, Maybe b) -> m ([a], b)
untilJust f = go
    where go = do
            x <- f
            case snd x of
              Nothing -> do
                (xs, b) <- go
                return (fst x : xs, b)
              Just b -> return ([], b)


runLogIter :: (MonadReader Opt m, MonadState SimState m) => m (String, Maybe SimStop)
runLogIter = do
  lgIter <- asks logIter
  nEvents' <- asks nEvents
  bend <- asks backend
  minLoss' <- asks minLoss
  verifyRC <- asks verifyReuseConstraint
  iStart <- use ssIter
  let runStep' :: (MonadReader Opt m, MonadState SimState m)
        => m (Float, Maybe SimStop)
      runStep' = do
        mbLoss <- runStep
        grid <- use ssGrid
        iTotal <- use ssIter
        return $ case mbLoss of
          Nothing -> (0, Just NaNLoss)
          Just loss
            | verifyRC && runExp bend (violatesReuseConstraint (A.use grid))
              -> (loss, Just ReuseConstraintViolated)
            | minLoss' > 0 && iStart > 50 && abs loss < minLoss'
              -> (loss, Just (NoLoss loss))
            | iTotal == nEvents'
              -> (loss, Just Success)
            | iTotal-iStart == lgIter
              -> (loss, Just Paused)
            | otherwise
              -> (loss, Nothing)
  (losses, simstop) <- untilJust runStep'
  i <- use ssIter
  report <- statePartM ssStats (statsReportLogIter i losses)
  return $ case simstop of
    Paused -> (report, Nothing)
    _ -> (report, Just simstop)

-- | Run 'logIter' steps in the simulator then
-- | print some statistics detailing the just-finished period.
runLogIterWrapper :: Opt -> SimState -> IO (Either SimState (SimState, SimStop))
runLogIterWrapper opts currentState = do
  let (liRes, newState) = runReader (runStateT runLogIter currentState) opts
  putStrLn $ fst liRes
  whenJust (snd liRes) print
  return $ case snd liRes of
    Nothing -> Left newState
    Just ss -> Right (newState, ss)

-- | Given random seed and simulation options, run the simulation
-- | and print out stats.
runSim :: Word64 -> Opt -> IO ()
runSim seed opts = do
  -- Execute 'nEvents' in the simulator.
  -- TODO graceful quit on CTRL-C
  -- TODO graceful quit on NaN loss
  startTime <- getCurrentTime
  let sState = runReader (mkSimState seed) opts
      fn = runLogIterWrapper opts
   -- Run the simulation until a 'simstop' occurs
  (sState', _simstop) <- loopM fn sState
  -- Print the final 'statistics' report
  let bkend = backend opts
      nCallsInProgress = runExp bkend $ boolSum (A.use (sState'^.ssGrid))
      i = view ssIter sState'
  putStrLn $ evalState (statsReportEnd nCallsInProgress i) (sState'^.ssStats)
  -- Print simulation duration and speed (in wall-clock time)
  endTime <- getCurrentTime
  let dt = diffUTCTime endTime startTime :: NominalDiffTime
      rate = fromRational $ fromIntegral i / toRational dt :: Double
  -- Duration/speed calc includes building the graphs
  -- (the 2 runN's; hopefully only 1 time ..)
  putStrLn $
    printf "\nSimulation duration: %s.\nProcessed %d events at %.0f events/second." (show dt) i rate
