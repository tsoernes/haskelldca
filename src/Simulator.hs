{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulator where

import Base
import Control.Arrow ((&&&))

import Agent

import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import Control.Monad.State

import Data.Array.Accelerate (Exp, Acc, Array, DIM0, DIM1, DIM2, DIM3, DIM4, Z(..), (:.)(..), constant, unindex1, index2, index3, (:.)(..), All(..), Any(..), Z(..), arrayShape, arraySize, Exp, slice)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Data.Maybe as M
import AccUtils (boolSum2, runExp, argpmax1, argpmax, unplain)
import Data.Maybe (isNothing)
import Data.RVar (sampleRVar)
import Data.Random (stdUniform)
import Prelude as P

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
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
import Opt
import Stats
  ( Stats
  , mkStats
  , statsEventAcceptNew
  , statsEventArrivalHoff
  , statsEventArrivalNew
  , statsEventEnd
  , statsEventRejectNew
  , statsReportEnd
  , statsReportLogIter
  )
import Text.Printf (printf)
import LensUtils

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

-- makeLenses ''SimState
makeClassy ''SimState

mkSimState :: Word64 -> Reader Opt SimState
mkSimState seed = do
  eg <- mkEventGen seed
  -- Get the first event that needs to be handled
  let (event, eg') = runState pop eg :: (Event, EventGen)
      g = mkGrid
      f = featureRep g
  return $ SimState g f event eg' mkStats mkAgent 0


getAction :: SimState -> (Exp (Maybe Ch), Frep)
getAction sstate = (mbch, frep)
  where
    grid = sstate ^. ssGrid
    ev = sstate ^. ssEvent
    eType = ev ^. evType
    cell = ev ^. evCell

    chs = A.acond (A.lift (isEnd eType))
        (inuseChs cell grid)
        (eligibleChs cell grid) :: A.Acc (A.Array A.DIM1 Int)

    -- Hopefully this is lazy so that 'selectAction' is not called if 'chs' is empty
    (_idxSh, _qval, _frep) = selectAction sstate chs
    ch = chs A.! _idxSh :: Exp Int
    --  :: Exp (A.Plain (Maybe Int))
    noCh = constant M.Nothing :: Exp (Maybe Ch)
    someCh = A.lift (M.Just ch) :: Exp (Maybe Ch)
    mbch = A.cond (A.null chs) noCh someCh
    frep = A.acond (A.null chs) (sstate ^. ssFrep) _frep


selectAction :: SimState -> Acc (Array DIM1 Int) -> (Exp DIM1, Exp Float, Frep)
selectAction sstate chs = (idx, qval, afrep)
  where
    -- Find the highest valued ch by running
    -- the corresponding afterstate feature representations of each ch through
    -- the function approximator (i.e. the neural network).
    grid = sstate ^. ssGrid :: Acc (Array DIM3 Bool)
    frep = sstate ^. ssFrep :: Acc (Array DIM3 Int)
    cell = sstate ^. ssEvent . evCell :: (Int, Int)
    eType = sstate ^. ssEvent . evType :: EType
    afreps = incAfterStateFreps grid frep cell eType chs :: Acc (Array DIM4 Int)
    af_sh = index2 (A.length chs) (A.lift (rOWS * cOLS * (cHANNELS + 1)))
    afreps_flat = A.reshape af_sh afreps
    afreps_flat_f = A.map A.fromIntegral afreps_flat
    -- Batch mode vector-vector product
    weights = sstate ^. ssAgent . wNet :: Acc (Array DIM1 Float)
    batch_weights = A.replicate (A.lift (Z :. A.length chs :. All)) weights
    qvals = A.flatten $ A.fold (+) 0 $ A.zipWith (*) afreps_flat_f batch_weights
    (idx, qval) = argpmax qvals
    -- The afterstate feature rep resulting from executing the best action
    afrep = slice afreps (A.lift (Z :. unindex1 idx :. All :. All :. All)) :: Frep


-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s); execute the action on the grid and return the resulting reward.
environmentStep ::
     (MonadReader Opt m, MonadState SimState m) => Maybe Ch -> m (A.Exp Int)
environmentStep act = do
  ev@(Event time eType cell) <- use ssEvent
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
    END _ _ -> statePartM ssStats statsEventEnd
  -- Generate further call events
  case eType of
    NEW -> do
      statePartM ssEventgen (generateNewEvent time cell)
      forM_ act (\ch -> do
           -- With probability ph, hand off the call to a
           -- nearby cell instead of terminating
           p <- statePartM (ssEventgen . gen) (sampleRVar stdUniform)
           ph <- asks hoffProb
           if p < ph
             then statePartM ssEventgen (generateHoffNewEvent time cell ch)
             else statePartM ssEventgen (generateEndEvent time cell ch))
    HOFF -> forM_ act (statePartM ssEventgen . generateHoffEndEvent time cell)
    END _ _ -> return ()
  -- Execute action, if any, on the grid and return call count as reward
  forM_ act (modifyPart ssGrid . executeAction ev)
  ssIter += 1
  boolSum2 <$> use ssGrid

-- | The inner loop which executes a single step in the simulator. First,
-- | the agent selects an action which is executed on the network grid. In
-- | return, the caller environment emits the next event for which an action
-- | must be selected, and a reward.
-- | Based on the reward, the agent adjusts its evaluation of the grid state
-- | prior to action. The so-called value function yields a
-- | numeric evaluation of an arbitrary grid state which can be thought
-- | of as the degree 
runStep :: (MonadReader Opt m, MonadState SimState m) => m ()
runStep
 = do
  (ambch, nextFrep) <- gets getAction
  -- ^ Select an action based on the observed state
  bend <- asks backend
  let mbch = runExp bend ambch :: Maybe Ch
  curFrep <- use ssFrep
  -- Execute the action in the environment (update grid; receive next event)
  -- and receive a reward
  reward <- environmentStep mbch
  -- Train the agent on the just received experience consisting of
  -- the state transition (in feature space) and a reward.
  loss <- statePartM ssAgent (backward curFrep (A.fromIntegral reward) nextFrep)
  -- Update the feature representation to match the current grid state
  -- (The frep was carried from when an action was selected to avoid recalculating it.)
  ssFrep .= nextFrep
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
      fn = runLogIterWrapper opts
  sState' <- foldM fn sState [0 .. nLogIters]
  -- Print the final 'statistics' report
  let stats = view ssStats sState' :: Stats
      bend = backend opts
      nCallsInProgress = runExp bend $ boolSum2 (view ssGrid sState')
      i = view ssIter sState'
  putStrLn $ statsReportEnd nCallsInProgress i stats
  -- .. and runtime + speed
  endTime <- getCurrentTime
  let dt = diffUTCTime endTime startTime :: NominalDiffTime
      rate = fromRational $ fromIntegral i / toRational dt :: Double
  putStrLn $
    printf
      "Simulation duration: %s. Processed %d events at %.0f events/second."
      (show dt)
      i
      rate