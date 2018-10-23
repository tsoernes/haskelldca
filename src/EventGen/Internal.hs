{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module EventGen.Internal where

import           Base
import           Control.Lens ( use, (+=), makeLenses )
import           Control.Monad.State.Strict ( MonadState )
import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Heap as Heap ( insert, empty, MinHeap )
import qualified Data.Map.Strict as Map ( Map, (!), delete, empty, insert )
import           Data.RVar ( getRandomDouble, getRandomWord64, sampleRVar )
import           Data.Random ( MonadRandom )
import           Data.Random.Distribution.Exponential ( exponential )
import           Data.Random.Internal.TH ()
import           Data.Random.Source ( monadRandom )
import           Data.Word ( Word64 )
import           LensUtils ( modifyPart, statePart )
import           System.Random.Mersenne.Pure64 ( PureMT, pureMT, randomDouble, randomWord64 )


data EventKey = EventKey
  { ekTime :: Double
  , ekId :: EventId
  } deriving (Eq, Show)

-- | Used to assure that the END event of a hand-off is handled before the HOFF part
instance Ord EventKey where
  e1 `compare` e2 =
    case ekTime e1 `compare` ekTime e2 of
      EQ -> ekId e1 `compare` ekId e2
      x -> x

-- | An Event Generator. It stores events in a min-heap and uses two auxiliary hash maps
-- | to allow efficient use of the following operations:
-- | 1: Retrieval of the event with the largest priority
-- | 2: The priority is the (inverse of) time stamp of the event. In case of call hand-off,
-- | the termination (END) event in the departure cell has larger priority than the
-- | service request (HOFF) in the arrival cell.
-- | 3: Channel reassignment. Is made possible due to efficient retrieval of stored END
-- | event based only on cell index and channel in use.
data EventGen = EventGen
  { _egId :: EventId
    -- ^ The smallest unused event ID.
    -- Since it is initialized at 0 (and only grows) it
    -- keeps track of how many events have been pushed.
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


_generateEndEvent ::
  (MonadRandom m, MonadState EventGen m) =>
  Double -> Cell -> Ch -> Double -> m ()
_generateEndEvent time cell ch lam = do
  dt <- sampleRVar (exponential (lam :: Double))
  push $ Event (time + dt) (END ch Nothing) cell


-- | Look up the value for a key 'k'; remove and return the value
mapRemove :: Ord k => k -> Map.Map k a -> (a, Map.Map k a)
mapRemove k mapp = (mapp Map.! k, Map.delete k mapp)
