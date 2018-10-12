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
import AccUtils
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Control.Lens (_1)
import Data.Array.Accelerate
  ( (:.)(..)
  , (:.)(..)
  , Acc
  , Scalar
  , All(..)
  , Array
  , DIM0
  , DIM1
  , DIM4
  , Exp
  , Exp
  , Z(..)
  , Z(..)
  , constant
  , fill
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

mkAgent :: (MonadReader Opt m) => m Agent
mkAgent = do
  bend <- asks backend
  let mk = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1))) 0.0
      w = run bend mk
      avgr = A.fromList Z [0.0] :: Scalar Float
  return (w, w, avgr)

-- | Given random seed, construct initial simulator state
mkSimState :: Word64 -> Reader Opt SimState
mkSimState seed = do
  eg <- mkEventGen seed
  g <- mkGrid
  ag <- mkAgent
  bend <- asks backend
  -- Retrieve the first event that needs to be handled
  let (event, eg') = runState pop eg :: (Event, EventGen)
      f = run bend $ featureRep $ A.use g
  return $ SimState g f event eg' mkStats ag 0

-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s). 
environmentStep :: (MonadReader Opt m, MonadState SimState m) => Maybe Ch -> m ()
environmentStep act = do
  (Event time eType cell) <- use ssEvent
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
  ssIter += 1

-- | Execute the action on the grid and return the resulting reward.
gridStep :: Exp Bool -> Exp Cell -> Exp (M.Maybe Ch) -> Acc Grid -> Acc (Scalar Int, Grid)
gridStep eIsEnd eCell act grid = tup
  where
  -- Execute action, if any, on the grid and return resulting call count as reward
  grid' = A.acond (M.isNothing act)
    grid
    (executeAction' eIsEnd eCell (M.fromJust act) grid)
  reward = boolSum grid
  tup = A.lift (A.unit reward, grid')

-- | Given current grid conditions and an event, select a channel.
-- | The channel in conjunction with the event specify an action.
-- | For NEW/HOFF events, the channel specifies which channel to assign to the call request.
-- | If there are no eligible channels, no action can be performed and 'Nothing' is returned.
-- | For END events, the channel specifies with channel
-- | to reassign to the channel of the terminating call.
-- | In addition, the frep of the afterstate corresponding to the channel is returned
-- | to avoid recomputing it when the action is later executed.
getAction' :: Acc (Scalar Cell) -> Acc (Scalar Bool) -> Acc Agent -> Acc Grid -> Acc Frep -> Acc (Array DIM0 (Maybe Ch), Frep)
getAction' (A0 cell) (A0 eIsEnd) = getAction cell eIsEnd

getAction :: Exp Cell -> Exp Bool -> Acc Agent -> Acc Grid -> Acc Frep -> Acc (Array DIM0 (Maybe Ch), Frep)
getAction cell eIsEnd agent grid frep = res
  where
    -- For NEW/HOFF events, the action space consists of the channels
    -- that are eligible in 'cell'.
    elig = eligibleChs' cell grid
    -- For END events, pick a channel in use to reassign to the channel that will
    -- terminate.
    inuse = inuseChs' cell grid
    chs = A.acond eIsEnd inuse elig :: A.Acc Chs
    A3 _idxSh _qval _nextFrep = selectAction cell eIsEnd chs agent grid frep
    -- (_idxSh, _qval, _nextFrep) = A.unlift saRes :: (Acc (Scalar DIM1), Acc (Scalar Float), Acc Frep)
    noCh = constant M.Nothing :: Exp (Maybe Ch)
    someCh = A.lift (M.Just $ chs A.! A.the _idxSh) :: Exp (Maybe Ch)
    res = A.acond (A.null chs)
      (A.lift (A.unit noCh, frep))
      (A.lift (A.unit someCh, _nextFrep))

-- | Find the highest valued ch by running
-- | the corresponding afterstate feature representations of each ch through
-- | the function approximator (i.e. the neural network).
-- | In other words, all possible futures at the next step are given
-- | a desirability measure and we select the action leading to the most desirable
-- | state next time-step. This approach is only possible in domains
-- | where the value of the next state is deterministic (or we know the distribution)
-- | given an action; otherwise you would compare actions directly using a state-action
-- | method such as Q-Learning or a policy neural network.
-- | TODO Improve naming and module-placement of getAction, selectAction, forward, backward
selectAction :: Exp Cell -> Exp Bool -> Acc (Array DIM1 Int) -> Acc Agent -> Acc Grid -> Acc Frep -> Acc (Scalar DIM1, Scalar Float, Frep)
selectAction cell eIsEnd chs agent grid frep = A.lift (A.unit idx, A.unit qval, afrep)
  where
    afreps = incAfterStateFreps cell eIsEnd chs grid frep :: Acc (Array DIM4 Int)
    aflat_sh = index2 (A.length chs) (A.lift (rOWS * cOLS * (cHANNELS + 1)))
    afreps_flat = A.reshape aflat_sh afreps
    afreps_flat_f = A.map A.fromIntegral afreps_flat
    -- Batch mode vector-vector inner dot product;
    -- the result of which is a Q-value for each ch.
    weights = agent^._1 :: Acc (Array DIM1 Float)
    batch_weights = A.replicate (A.lift (Z :. A.length chs :. All)) weights
    -- In this domain, the grid transitions deterministically given an event
    -- and a channel as action. Th
    qvals = A.flatten $ A.fold (+) 0 $ A.zipWith (*) afreps_flat_f batch_weights
    -- Strictly greedy action selection through argmax. This is where you
    -- would perform exploration, such as epsilon-greedy or Boltzmann exploration,
    -- instead of picking the action with the highest action value (Q-val).
    (idx, qval) = argpmax qvals
    -- The afterstate feature rep resulting from executing the best action
    afrep_sh = A.lift (Z :. unindex1 idx :. All :. All :. All)
    afrep = slice afreps afrep_sh :: Acc Frep
