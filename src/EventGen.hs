{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module EventGen
  ( EventGen
  , generateNewEvent
  , generateHoffNewEvent
  , generateEndEvent
  , generateHoffEndEvent
  , reassign
  , mkEventGen
  , pop
  , push
  ) where

import           Base
import           Control.Monad (forM_, foldM)
import           Control.Monad.Reader (Reader, asks, runReader, lift)
import           Control.Monad.State.Lazy (State, MonadState, StateT, get, put, modify', state, execStateT)
import           Data.Functor.Identity (Identity)
import qualified Data.Heap as Heap
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Random (MonadRandom, sampleState)
import           Data.Random.Distribution.Exponential (exponential)
import           Data.Random.Distribution.Uniform (integralUniform)
import           Data.Random.Lift (Lift)
import           Gridneighs (getNeighs)
import           Control.Lens (makeLenses, use, zoom, at, _1, _2, assign, view, set)
import           Opt
import           System.Random (StdGen, mkStdGen, randomR)

data EventGen = EventGen
  { _egId :: EventId -- Last used event ID
  , _queue :: Heap.MinHeap EventKey -- Min-heap of event-identifiers sorted on event times
  , _events :: Map.Map EventId Event -- Mapping from event IDs to event structs
  , _endIds :: Map.Map (Cell, Ch) EventId -- Mapping from cell-channel pairs to END event IDs
  }

makeLenses ''EventGen


mkEventGen :: StateT StdGen (Reader Opt) EventGen
mkEventGen = do
  gen <- get
  let mke = EventGen 0 Heap.empty Map.empty Map.empty
      addEvent :: () -> Cell -> StateT (StdGen, EventGen) (Reader Opt) ()
      addEvent _ = generateNewEvent 0.0
  (gen', eg) <- lift $ execStateT (foldM addEvent () gridIdxs) (gen, mke)
  put gen'
  return eg

push :: Double -> EType -> Cell -> Maybe Ch -> Maybe Cell -> State EventGen ()
push time etype cell endCh hoffCell = do
  eId <- use egId :: State EventGen Int
  let event = Event eId time etype cell endCh hoffCell
  zoom events . modify' $ Map.insert eId event
  forM_
    endCh
    (\ch -> do
       zoom queue . modify' $ Heap.insert EventKey {ekTime = time, ekId = eId}
       zoom endIds . modify' $ Map.insert (cell, ch) eId)
  return ()

-- | Retrieve the highest priority event from the event generator
pop :: State EventGen Event
pop
  -- Pop an identifier from the heap and retrieve the corresponding event
  -- from the hashmap. Then delete the it from the hashmaps.
  = do
  eKey <- zoom queue $ state (fromJust . Heap.view)
  let eId = ekId eKey
  event <-
    zoom events $ do
      event <- fmap (Map.! eId) get
      modify' $ Map.delete eId
      return event
  forM_
    (view evEndCh event)
    (\ch -> zoom endIds . modify' $ Map.delete (view evCell event, ch))
  return event

-- | Correct an event (and its keys) to reflect a channel reassignment
reassign :: Cell -> Ch -> Ch -> State EventGen ()
reassign cell fromCh toCh = do
  id <-
    zoom endIds $ do
      id <- state $ mapRemove (cell, fromCh)
      modify' $ Map.insert (cell, toCh) id
      return id
  zoom events . modify' $ Map.adjust (set evEndCh (Just toCh)) id
  return ()

generateNewEvent :: Double -> Cell -> StateT (StdGen, EventGen) (Reader Opt) ()
generateNewEvent time cell = do
  lam <- asks callRate
  dt <- zoom _1 $ exponentialSt (1.0 / lam :: Double)
  zoom _2 . return $ push (time + dt) NEW cell Nothing Nothing
  return ()

generateHoffNewEvent ::
     Double -> Cell -> Ch -> StateT (StdGen, EventGen) (Reader Opt) ()
generateHoffNewEvent time cell ch = do
  lam <- asks callDurNew
  dt <- zoom _1 $ exponentialSt (lam :: Double)
  let neighs = getNeighs 2 cell False
  neigh_i <- zoom _1 $ state $ randomR (0, length neighs - 1)
  let toCell = neighs !! neigh_i
      t = time + dt
  -- A termination event (END) immediately succeeded
  -- by an arrival event (HOFF) in a neighboring cell
  zoom _2 . return $ do
    push t END cell (Just ch) (Just toCell)
    push t HOFF toCell Nothing Nothing
  return ()

generateEndEvent ::
     Double -> Cell -> Ch -> StateT (StdGen, EventGen) (Reader Opt) ()
generateEndEvent time cell ch = do
  lam <- asks callDurNew
  dt <- zoom _1 $ exponentialSt (lam :: Double)
  zoom _2 . return $ push (time + dt) END cell (Just ch) Nothing
  return ()

generateHoffEndEvent ::
     Double -> Cell -> Ch -> StateT (StdGen, EventGen) (Reader Opt) ()
generateHoffEndEvent time cell ch = do
  lam <- asks callDurHoff
  dt <- zoom _1 $ exponentialSt (lam :: Double)
  zoom _2 . return $ push (time + dt) END cell (Just ch) Nothing
  return ()
  
-- | Look up the value for a key 'k'; remove and return the value
mapRemove :: Ord k => k -> Map.Map k a -> (a, Map.Map k a)
mapRemove k map = (map Map.! k, Map.delete k map)

-- | Given scale parameter 'lambda', sample a double from the exponential distribution
exponentialSt :: (MonadRandom (State s), MonadState s m) => Double -> m Double
exponentialSt = state . sampleState . exponential
