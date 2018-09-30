{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulator where

import Base
import Control.Arrow ((&&&))

import Agent
-- import Control.Lens (Lens', lens, makeLenses, over, set, use, view, (<%=))
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Text.Printf (printf)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import Control.Monad.State
import Data.Array.Accelerate ((:.)(..), All(..), Z(..))
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native (run)
import Data.Maybe (isNothing)
import Data.RVar (sampleRVar)
import Data.Random (stdUniform)
import Data.Word (Word64)
import EventGen
  ( EventGen
  , gen
  , generateEndEvent
  , generateHoffEndEvent
  , generateHoffNewEvent
  , generateNewEvent
  , mkEventGen
  , pop
  )
import Gridfuncs
  ( afterstates
  , argpmax
  , eligibleChs
  , featureRep
  , inuseChs
  , mkFreps
  , runExp
  , vvMul
  , boolSum2, executeAction, featureRep, mkGrid)
import Opt
import Stats
  ( Stats
  , mkStats
  , statsEventAcceptNew
  , statsEventArrivalHoff
  , statsEventArrivalNew
  , statsEventEnd
  , statsEventRejectNew
  , statsReportLogIter
  , statsReportEnd
  )
import Utils

data SimState = SimState
  { _ssGrid :: Grid
  , _ssFrep :: Frep -- Strictly speaking, the frep is not a part of the simulator
  -- The next event for which an action must be selected and executed on 'grid'
  , _ssEvent :: Event
  , _ssEventgen :: EventGen
  , _ssStats :: Stats
  , _ssAgent :: Agent
  , _ssIter :: Int -- Number of executed iterations
  }

makeLenses ''SimState

mkSimState :: Word64 -> Reader Opt SimState
mkSimState seed = do
  eg <- mkEventGen seed
  -- Get the first event that needs to be handled
  let (event, eg') = runState pop eg :: (Event, EventGen)
      g = mkGrid
      f = featureRep g
  return $ SimState g f event eg' mkStats mkAgent 0

getAction :: SimState -> (Maybe Ch, Frep)
getAction sstate = (mbch, frep)
  where
    ev = view ssEvent sstate
    grid = view ssGrid sstate
    cell = view evCell ev
    chs =
      if view evType ev == END
        then inuseChs cell grid
        else eligibleChs cell grid
    (mbch, frep) =
      if runExp (A.null chs)
        then (Nothing, view ssFrep sstate)
        else let (ch, frep) = selectAction sstate chs
              in (Just ch, frep)
        -- TO BE USED IF GETQVALS CODE IS FIXED TO USE ACC INSTEAD OF LIST
        -- else let (qvals, freps) = getQvals sstate chs
        --          (idxSh, _) = argpmax qvals
        --          idx = runExp $ unindex1 idxSh
        --          ch = runExp $ chs ! idxSh
        --       in ( P.Just ch
        --          , slice freps (constant (Z :. idx :. All :. All :. All)))

-- TODO VERY SLOW. THIS SHOULD BE REIMPLEMENTED USING 4D ACC arrays (no or less running)
-- instead of lists
selectAction :: SimState -> Chs -> (Ch, Frep)
selectAction sState chs = (chs' !! best_i, best_frep)
  where
    chs' = A.toList $ run chs :: [Int]
    ass =
      afterstates
        (sState ^. ssGrid)
        (sState ^. ssEvent . evCell)
        (sState ^. ssEvent . evType)
        chs
    (best_i, best_frep, _) = foldl fn (-1, undefined, -1.0) chs'
    fn :: (Int, Frep, Float) -> Int -> (Int, Frep, Float)
    fn acc@(acc_i, acc_frep, acc_qval) i =
      if qval > acc_qval
        then (i, frep, qval)
        else acc
      where
        grid = A.slice ass (A.constant (Z :. i :. All :. All :. All)) :: Grid
        frep = featureRep grid
        qval = runExp $ forward frep (sState ^. ssAgent) :: Float

-- -- | Get the Q-values for the given actions. Also returns the corresponding freps since.
-- -- | TODO implement HLA
-- -- | TODO implementation abandoned due to not knowing how to populate a 4D acc array (freps)
-- -- | by filling it with one 3D entry (a frep) at a time.
-- getQvals :: SimState -> Chs -> (Acc (Array DIM1 Float), Freps)
-- getQvals sstate chs = undefined
--     -- P.foldl fn (zqvals, zfreps) (indexed chs)
--     -- P.map ((map snd) . fst) $
--     -- fn :: (Acc (Array DIM1 Float), Freps) -> (Exp DIM1, Exp Ch) -> (Acc (Array DIM1 Float), Freps)
--     -- fn (qvals, freps) = undefined
--   where
--     l = length chs
--     zfreps = mkFreps l
--     zqvals = fill (index1 l) 0 :: Acc (Array DIM1 Float)
--     freps = P.map (\i -> featureRep $ afterstates (chs ! i))
--     -- freps = permute const zfreps idxTrans chs
--     -- idxTrans :: Exp DIM1 -> Exp DIM4
--     -- idxTrans sh =

-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s); execute the action on the grid and return the resulting reward.
environmentStep ::
     (MonadReader Opt m, MonadState SimState m) => Maybe Ch -> m (A.Exp Int)
environmentStep act = do
  ev@(Event time eType cell _ _) <- use ssEvent
  -- Log call events to Stats record
  case eType of
    NEW -> do
      statePartM ssStats statsEventArrivalNew
      case act of
        Just _ -> statePartM ssStats statsEventAcceptNew
        Nothing -> statePartM ssStats statsEventRejectNew
    HOFF -> do
      statePartM ssStats statsEventArrivalHoff
      when (isNothing act) $ statePartM ssStats statsEventRejectNew
    END -> statePartM ssStats statsEventEnd
  -- Generate further call events
  case eType of
    NEW -> do
      statePartM ssEventgen (generateNewEvent time cell)
      forM_
        act
        (\ch
          -- With probability ph, hand off the call to a
          -- nearby cell instead of terminating
          -> do
           p <- statePartM (ssEventgen . gen) (sampleRVar stdUniform)
           ph <- asks hoffProb
           if p < ph
             then statePartM ssEventgen (generateHoffNewEvent time cell ch)
             else statePartM ssEventgen (generateEndEvent time cell ch))
    HOFF -> forM_ act (statePartM ssEventgen . generateHoffEndEvent time cell)
    END -> return ()
  -- Execute action, if any, on the grid and return call count as reward
  forM_ act (modifyPart ssGrid . executeAction ev)
  ssIter += 1
  boolSum2 <$> use ssGrid

runStep :: (MonadReader Opt m, MonadState SimState m) => m ()
runStep = do
  ss <- get
  -- Select an action based on the observed state
  let (mbch, nextFrep) = getAction ss
  -- Execute the action in the environment and receive a reward
  reward <- environmentStep mbch
  -- Train the agent
  loss <- statePartM ssAgent (backward (ss^.ssFrep) (fromIntegral reward) nextFrep)
  -- Update the part of the state that did not get updated in 'environmentStep'
  put $ set ssFrep nextFrep ss
  return ()

runLogIter :: (MonadReader Opt m, MonadState SimState m) => m String
runLogIter = do
  li <- asks logIter
  forM_ [0 .. li - 1] (const runStep)
  i <- use ssIter
  statePartM ssStats (statsReportLogIter i)

runLogIterWrapper :: Opt -> SimState -> Int -> IO SimState 
runLogIterWrapper opts currentState _ = do
  let (liRes, newState) = runReader (runStateT runLogIter currentState) opts
  putStrLn liRes
  return newState

runSim :: Word64 -> Opt -> IO ()
runSim seed opts = do
  startTime <- getCurrentTime
  let sState = runReader (mkSimState seed) opts
      nLogIters = nEvents opts `div` logIter opts
  sState' <- foldM (runLogIterWrapper opts) sState [0..nLogIters]
  -- Print the final 'statistics' report
  let stats = view ssStats sState' :: Stats
      nCallsInProgress = runExp $ boolSum2 (view ssGrid sState')
      i = view ssIter sState'
  putStrLn $ statsReportEnd nCallsInProgress i stats
  -- .. and runtime + speed
  endTime <- getCurrentTime
  let dt = diffUTCTime endTime startTime :: NominalDiffTime
      rate = fromRational $ fromIntegral i / toRational dt :: Double
      i = view ssIter sState'
  putStrLn $ printf "Simulation duration: %s. Processed %d events at %.0f events/second." (show dt) i rate