{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module EventGen where
  -- ( EventGen
  -- , generateNewEvent
  -- , generateHoffNewEvent
  -- , generateEndEvent
  -- , generateHoffEndEvent
  -- , reassign
  -- , mkEventGen
  -- , pop
  -- , push
  -- , statePart
  -- , modifyPart
  -- ) where

import Base
import Control.Lens
  ( ALens'
  , Lens'
  , Zoom(zoom)
  , ( #%~ )
  , ( #~ )
  , (+=)
  , (.=)
  , (^#)
  , (^.)
  , makeLenses
  , over
  , set
  , use
  , uses
  , view
  )
import Control.Monad (foldM, forM_)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Lazy
  ( MonadState(state)
  , State
  , StateT
  , execStateT
  , gets
  , modify'
  , runStateT
  )
import qualified Data.Heap as Heap (MinHeap, empty, insert, null, view)
import qualified Data.Map.Strict as Map
  ( Map
  , (!)
  , (!?)
  , adjust
  , delete
  , empty
  , insert
  , null
  )
import Data.Maybe (fromJust, isNothing)
import Data.RVar (getRandomDouble, getRandomWord64, sampleRVar)
import Data.Random (MonadRandom, uniform)
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Internal.TH ()
import Data.Random.Source (monadRandom)
import Data.Word (Word64)
import Debug.Trace
import Gridneighs (getNeighs)
import Opt (Opt(callDurHoff, callDurNew, callRate))
import System.Random.Mersenne.Pure64
  ( PureMT
  , pureMT
  , randomDouble
  , randomWord64
  )
import LensUtils

data EventGen = EventGen
    -- First unused event ID.
    -- Since it is initialized at 0 it keeps track of how many events have been pushed.
  { _egId :: EventId
  , _queue :: Heap.MinHeap EventKey -- Min-heap of event-identifiers sorted on event times
  , _events :: Map.Map EventId Event -- Mapping from event IDs to event structs
  , _endIds :: Map.Map (Cell, Ch) EventId -- Mapping from cell-channel pairs to END event IDs
  , _gen :: PureMT
  }

-- | For debugging only: Check that the Heap and the two Maps are all empty
isEmpty :: EventGen -> Bool
isEmpty (EventGen _ q evs eids _) = Heap.null q && Map.null evs && Map.null eids

mke :: Word64 -> EventGen
mke = EventGen 0 Heap.empty Map.empty Map.empty . pureMT

makeLenses ''EventGen

$(monadRandom
    [d|

  instance Monad m => MonadRandom (StateT EventGen m) where
          getRandomWord64 = statePart gen randomWord64
          getRandomDouble = statePart gen randomDouble
  |])

mkEventGen :: (MonadReader Opt m) => Word64 -> m EventGen
mkEventGen seed = execStateT fn (mke seed)
  where
    fn ::
         (MonadRandom (StateT EventGen m), MonadReader Opt m)
      => (StateT EventGen m) ()
    fn = mapM_ (generateNewEvent 0.0) gridIdxs

-- COMPILES, although you get:
--  Illegal instance declaration for
--    ‘MonadRandom (StateT EventGen m)’
--    (All instance types must be of the form (T a1 ... an)
--     where a1 ... an are *distinct type variables*,
--     and each type variable appears at most once in the instance head.
--     Use FlexibleInstances if you want to disable this.)
--  In the instance declaration for ‘MonadRandom (StateT EventGen m_a1cSc)’ (haskell-dante)
-- | Push an event into the event generator.
push :: (MonadState EventGen m) => Event -> m ()
push event@(Event time etype cell) = do
  eId <- use egId
  modifyPart events $ Map.insert eId event
  modifyPart queue $ Heap.insert EventKey {ekTime = time, ekId = eId}
  case etype of
    END ch _ -> modifyPart endIds $ Map.insert (cell, ch) eId
    _ -> return ()
  egId += 1
  return ()

-- | Retrieve the highest priority event from the event generator.
-- Throws an error if the queue is empty.
pop :: (MonadState EventGen m) => m Event
pop
  -- Pop an identifier from the heap and retrieve the corresponding event
  -- from the hashmap. Then delete the it from the hashmaps.
 = do
  queueIsNull <- uses queue null
  -- Pass queue through trace OP
  -- let q = trace ("Queue is empty before pop: " ++ show queueIsNull) queue
  eKey <- statePart queue $ fromJust . Heap.view
  let eId = ekId eKey
  event <- fromJust <$> uses events (Map.!? eId)
  -- Pass event through trace OP
  -- let event' =
  --       fromJust $
  --       trace
  --         ("Event in map is nothing (before fromJust): " ++
  --          show (isNothing event'))
  --         event
  modifyPart events $ Map.delete eId
  case event ^. evType of
    END ch _ -> modifyPart endIds (Map.delete (view evCell event, ch))
    _ -> return ()
  return event

-- | Correct an event (and its keys) to reflect a channel reassignment
reassign :: (MonadState EventGen m) => Cell -> Ch -> Ch -> m ()
reassign cell fromCh toCh = do
  eId <-
    statePartM endIds $ do
      eId <- state $ mapRemove (cell, fromCh)
      modify' $ Map.insert (cell, toCh) eId
      return eId
  modifyPart events $ Map.adjust (\ev -> set evType (END toCh (hoffCell $ view evType ev)) ev)  eId
  return ()

generateNewEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m)
  => Double
  -> Cell
  -> m ()
generateNewEvent time cell = do
  lam <- asks callRate
  dt <- sampleRVar (exponential (1 / lam :: Double))
  _ <- push $ Event (time + dt) NEW cell
  return ()

-- | Hand off a call to another cell. A hand-off consists of two call events:
-- | A termination event (END) immediately succeeded
-- | by an arrival event (HOFF) in a neighboring cell.
-- | In this context, 'immediately succeeded' means that the two events
-- | have the same time ('evTime') but the END event is guaranteed to pop first
-- | due to EventKey's Ord class implementation.
generateHoffNewEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m)
  => Double -- Time of simultaneous departure from departure cell and arrival at neighbor
  -> Cell -- Departure cell
  -> Ch -- Channel to use while in departure cell
  -> m ()
generateHoffNewEvent time cell ch = do
  lam <- asks callDurNew
  dt <- sampleRVar (exponential (lam :: Double))
  let neighs = getNeighs 2 cell False
  neigh_i <- sampleRVar $ uniform 0 (length neighs - 1)
  let toCell = neighs !! neigh_i
      t = time + dt
  -- A problem with the EType data type is revealed here.
  -- Some events have both an end channel and hoff cell.
  _ <- push $ Event t (END ch (Just toCell)) cell 
  _ <- push $ Event t HOFF toCell 
  return ()

_generateEndEvent ::
     (MonadState EventGen m, MonadRandom m)
  => Double
  -> Cell
  -> Ch
  -> Double
  -> m ()
_generateEndEvent time cell ch lam = do
  dt <- sampleRVar (exponential (lam :: Double))
  _ <- push $ Event (time + dt) (END ch Nothing) cell
  return ()

generateEndEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m)
  => Double
  -> Cell
  -> Ch
  -> m ()
generateEndEvent time cell ch =
  asks callDurNew >>= _generateEndEvent time cell ch

generateHoffEndEvent ::
     (MonadState EventGen m, MonadReader Opt m, MonadRandom m)
  => Double
  -> Cell
  -> Ch
  -> m ()
generateHoffEndEvent time cell ch =
  asks callDurHoff >>= _generateEndEvent time cell ch

-- | Look up the value for a key 'k'; remove and return the value
mapRemove :: Ord k => k -> Map.Map k a -> (a, Map.Map k a)
mapRemove k mapp = (mapp Map.! k, Map.delete k mapp)

