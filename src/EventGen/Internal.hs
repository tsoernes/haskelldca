{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module EventGen.Internal where

import           Base ( evCell, evType, Cell, Ch, EType(END), Event(Event), EventId, EventKey(..) )
import           Control.Lens ( (^.), use, uses, view, (+=), makeLenses )
import           Control.Monad.State.Lazy ( StateT, MonadState )
import qualified Data.Heap as Heap ( insert, view, empty, MinHeap )
import qualified Data.Map.Strict as Map ( Map, (!), (!?), delete, empty, insert )
import           Data.Maybe ( fromJust )
import           Data.RVar ( getRandomDouble, getRandomWord64, sampleRVar )
import           Data.Random ( MonadRandom )
import           Data.Random.Distribution.Exponential ( exponential )
import           Data.Random.Internal.TH ()
import           Data.Random.Source ( monadRandom )
import           Data.Word ( Word64 )
import           LensUtils ( modifyPart, statePart )
import           System.Random.Mersenne.Pure64 ( PureMT, pureMT, randomDouble, randomWord64 )


data EventGen = EventGen
    -- First unused event ID.
    -- Since it is initialized at 0 it keeps track of how many events have been pushed.
  { _egId :: EventId
  , _egQueue :: Heap.MinHeap EventKey -- Min-heap of event-identifiers sorted on event times
  , _egEvents :: Map.Map EventId Event -- Mapping from event IDs to event structs
  , _egEndIds :: Map.Map (Cell, Ch) EventId -- Mapping from cell-channel pairs to END event IDs
  , _egGen :: PureMT
  }

makeLenses ''EventGen

$(monadRandom
    [d|

  instance Monad m => MonadRandom (StateT EventGen m) where
          getRandomWord64 = statePart egGen randomWord64
          getRandomDouble = statePart egGen randomDouble
  |])

_mkEventGen :: Word64 -> EventGen
_mkEventGen = EventGen 0 Heap.empty Map.empty Map.empty . pureMT


-- | Push an event into the event generator.
push :: (MonadState EventGen m) => Event -> m ()
push event@(Event time etype cell) = do
  eId <- use egId
  modifyPart egEvents $ Map.insert eId event
  modifyPart egQueue $ Heap.insert EventKey {ekTime = time, ekId = eId}
  case etype of
    END ch _ -> modifyPart egEndIds $ Map.insert (cell, ch) eId
    _ -> return ()
  egId += 1
  return ()

-- | Retrieve the highest priority event from the event generator.
-- | Throws an error if the queue is empty.
pop :: (MonadState EventGen m) => m Event
pop
  -- Pop an identifier from the heap and retrieve the corresponding event
  -- from the hashmap. Then delete the it from the hashmaps.
 = do
  queueIsNull <- uses egQueue null
  -- Pass queue through trace OP
  -- let q = trace ("Queue is empty before pop: " ++ show queueIsNull) queue
  eKey <- statePart egQueue $ fromJust . Heap.view
  let eId = ekId eKey
  event <- fromJust <$> uses egEvents (Map.!? eId)
  -- Pass event through trace OP
  -- let event' =
  --       fromJust $
  --       trace
  --         ("Event in map is nothing (before fromJust): " ++
  --          show (isNothing event'))
  --         event
  modifyPart egEvents $ Map.delete eId
  case event ^. evType of
    END ch _ -> modifyPart egEndIds (Map.delete (view evCell event, ch))
    _ -> return ()
  return event

_generateEndEvent time cell ch lam = do
  dt <- sampleRVar (exponential (lam :: Double))
  _ <- push $ Event (time + dt) (END ch Nothing) cell
  return ()

-- | Look up the value for a key 'k'; remove and return the value
mapRemove :: Ord k => k -> Map.Map k a -> (a, Map.Map k a)
mapRemove k mapp = (mapp Map.! k, Map.delete k mapp)
