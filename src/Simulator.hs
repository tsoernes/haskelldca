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

-- makeClassy ''SimState
mkSimState :: Word64 -> Reader Opt SimState
mkSimState seed = do
  eg <- mkEventGen seed
  -- Get the first event that needs to be handled
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
           -- With probability ph, hand off the call to a
           -- nearby cell instead of terminating
          -> do
           p <- statePartM (ssEventgen . egGen) (sampleRVar stdUniform)
           ph <- asks hoffProb
           if p < ph
             then statePartM ssEventgen (generateHoffNewEvent time cell ch)
             else statePartM ssEventgen (generateEndEvent time cell ch))
    HOFF -> forM_ act (statePartM ssEventgen . generateHoffEndEvent time cell)
    END _ _ -> return ()
  -- Execute action, if any, on the grid and return call count as reward
  forM_ act (modifyPart ssGrid . executeAction ev)
  ssIter += 1
  boolSum <$> use ssGrid

getAction :: SimState -> (Exp (Maybe Ch), Frep)
getAction sstate = (mbch, frep)
  where
    grid = sstate ^. ssGrid
    ev = sstate ^. ssEvent
    eType = ev ^. evType
    cell = ev ^. evCell
    chs =
      A.acond (A.lift (isEnd eType)) (inuseChs cell grid) (eligibleChs cell grid) :: A.Acc (A.Array A.DIM1 Int)
    -- Hopefully this is lazy so that 'selectAction' is not called if 'chs' is empty
    (_idxSh, _qval, _frep) = selectAction sstate chs
    noCh = constant M.Nothing :: Exp (Maybe Ch)
    someCh = A.lift (M.Just $ chs A.! _idxSh) :: Exp (Maybe Ch)
    mbch = A.cond (A.null chs) noCh someCh
    frep = A.acond (A.null chs) (sstate ^. ssFrep) _frep

selectAction :: SimState -> Acc (Array DIM1 Int) -> (Exp DIM1, Exp Float, Frep)
selectAction sstate chs = (idx, qval, afrep)
    -- Find the highest valued ch by running
    -- the corresponding afterstate feature representations of each ch through
    -- the function approximator (i.e. the neural network).
  where
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
