{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulator where

import Base
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, Reader, asks)
import Control.Monad.State
import AccUtils (argpmax, boolSum)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
  ( (:.)(..)
  , (:.)(..)
  , Acc
  , All(..)
  , Array
  , DIM1
  , DIM3
  , DIM4
  , Exp
  , Exp
  , Z(..)
  , Z(..)
  , constant
  , index2
  , slice
  , unindex1
  )
import qualified Data.Array.Accelerate.Data.Maybe as M
import Data.Maybe (isNothing)
import Data.RVar (sampleRVar)
import Data.Random (stdUniform)
import Prelude as P
import Data.Word (Word64)
import EventGen
import Gridfuncs
import Opt
import Stats
import LensUtils

-- | The state of the simulator any given time. This does not correspond to the
-- | state in a caller environment defined as a Markov Decision Process
-- | (which is the definition used in the context of Reinforcement Learning),
-- | which would only contain the grid and the event to be processed.
-- | In the MDP, state transitions are non-deterministic (only) due to
-- | the randomness of the event generation.
data SimState = SimState
  { _ssGrid :: Grid
  , _ssFrep :: Frep
  , _ssEvent :: Event -- The next event for which an action must be selected and executed
  , _ssEventgen :: EventGen
  , _ssStats :: Stats
  , _ssAgent :: Agent
  , _ssIter :: Int -- Number of executed iterations
  }

makeLenses ''SimState

-- | Given random seed, construct initial simulator state
mkSimState :: Word64 -> Reader Opt SimState
mkSimState seed = do
  eg <- mkEventGen seed
  -- Retrieve the first event that needs to be handled
  let (event, eg') = runState pop eg :: (Event, EventGen)
      g = mkGrid
      f = featureRep g
  return $ SimState g f event eg' mkStats mkAgent 0

-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s); execute the action on the grid and return the resulting reward.
environmentStep :: (MonadReader Opt m, MonadState SimState m) => Maybe Ch -> m (A.Exp Int)
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
      forM_
        act
        (\ch
          -> do
           p <- statePartM (ssEventgen . egGen) (sampleRVar stdUniform)
           -- With probability ph, schedule a hand off to a
           -- nearby cell instead of a regular call termination
           ph <- asks hoffProb
           if p < ph
             then statePartM ssEventgen (generateHoffNewEvent time cell ch)
             else statePartM ssEventgen (generateEndEvent time cell ch))
    HOFF -> forM_ act (statePartM ssEventgen . generateHoffEndEvent time cell)
    END _ _ -> return ()
  -- Execute action, if any, on the grid and return resulting call count as reward
  forM_ act (modifyPart ssGrid . executeAction ev)
  ssIter += 1
  boolSum <$> use ssGrid

-- | Given current grid conditions and an event, select a channel.
-- | The channel in conjunction with the event specify an action.
-- | For NEW/HOFF events, the channel specifies which channel to assign to the call request.
-- | If there are no eligible channels, no action can be performed and 'Nothing' is returned.
-- | For END events, the channel specifies with channel
-- | to reassign to the channel of the terminating call.
-- | In addition, the frep of the afterstate corresponding to the channel is returned
-- | to avoid recomputing it when the action is later executed.
getAction :: SimState -> (Exp (Maybe Ch), Frep)
getAction sstate = (mbch, frep)
  where
    grid = sstate ^. ssGrid
    ev = sstate ^. ssEvent
    eType = ev ^. evType
    cell = ev ^. evCell
    elig = eligibleChs cell grid
    inuse = inuseChs cell grid
    chs = A.acond (A.lift (isEnd eType)) inuse elig :: A.Acc (A.Array A.DIM1 Int)
    -- Hopefully this is lazy so that 'selectAction' is not called if 'chs' is empty
    (_idxSh, _qval, _frep) = selectAction sstate chs
    noCh = constant M.Nothing :: Exp (Maybe Ch)
    someCh = A.lift (M.Just $ chs A.! _idxSh) :: Exp (Maybe Ch)
    mbch = A.cond (A.null chs) noCh someCh
    frep = A.acond (A.null chs) (sstate ^. ssFrep) _frep

-- | Find the highest valued ch by running
-- | the corresponding afterstate feature representations of each ch through
-- | the function approximator (i.e. the neural network).
-- | TODO Improve naming and placement of getAction, selectAction, forward, backward
selectAction :: SimState -> Acc (Array DIM1 Int) -> (Exp DIM1, Exp Float, Frep)
selectAction sstate chs = (idx, qval, afrep)
  where
    grid = sstate ^. ssGrid :: Acc (Array DIM3 Bool)
    frep = sstate ^. ssFrep :: Acc (Array DIM3 Int)
    cell = sstate ^. ssEvent . evCell :: (Int, Int)
    eType = sstate ^. ssEvent . evType :: EType
    afreps = incAfterStateFreps grid frep cell eType chs :: Acc (Array DIM4 Int)
    aflat_sh = index2 (A.length chs) (A.lift (rOWS * cOLS * (cHANNELS + 1)))
    afreps_flat = A.reshape aflat_sh afreps
    afreps_flat_f = A.map A.fromIntegral afreps_flat
    -- Batch mode vector-vector inner dot product;
    -- the result of which is a Q-value for each ch.
    weights = sstate ^. ssAgent . wNet :: Acc (Array DIM1 Float)
    batch_weights = A.replicate (A.lift (Z :. A.length chs :. All)) weights
    -- In this domain, the grid transitions deterministically given an event
    -- and a channel as action. Th
    qvals = A.flatten $ A.fold (+) 0 $ A.zipWith (*) afreps_flat_f batch_weights
    -- Strictly greedy action selection through argmax. This is where you
    -- would perform exploration, such as epsilon-greedy or Boltzmann exploration,
    -- instead of picking the action with the highest action value (Q-val).
    (idx, qval) = argpmax qvals
    -- The afterstate feature rep resulting from executing the best action
    afrep = slice afreps (A.lift (Z :. unindex1 idx :. All :. All :. All)) :: Frep
