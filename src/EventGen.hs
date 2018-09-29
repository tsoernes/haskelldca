{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

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
  , statePart
  , modifyPart
  ) where

import Base
    ( Cell,
      Ch,
      EType(..),
      Event(Event),
      EventId,
      EventKey(..),
      evCell,
      evEndCh,
      gridIdxs )
import Control.Lens
    ( Lens', use, view, over, set, makeLenses, Zoom(zoom) )
import Control.Monad ( foldM, forM_ )
import Control.Monad.Reader ( MonadReader, asks )
import Control.Monad.State.Lazy
    ( MonadState(state), State, StateT, execStateT, gets, modify' )
import qualified Data.Heap as Heap ( MinHeap, empty, insert, view )
import qualified Data.Map.Strict as Map
    ( Map, (!), adjust, delete, empty, insert )
import Data.Maybe ( fromJust )
import Data.RVar ( getRandomDouble, getRandomWord64, sampleRVar )
import Data.Random ( MonadRandom, uniform )
import Data.Random.Distribution.Exponential ( exponential )
import Data.Random.Internal.TH ()
import Data.Random.Source ( monadRandom )
import Data.Word ( Word64 )
import Gridneighs ( getNeighs )
import Opt ( Opt(callDurHoff, callDurNew, callRate) )
import System.Random.Mersenne.Pure64
    ( PureMT, pureMT, randomDouble, randomWord64 )


data EventGen = EventGen
  { _egId :: EventId -- Last used event ID
  , _queue :: Heap.MinHeap EventKey -- Min-heap of event-identifiers sorted on event times
  , _events :: Map.Map EventId Event -- Mapping from event IDs to event structs
  , _endIds :: Map.Map (Cell, Ch) EventId -- Mapping from cell-channel pairs to END event IDs
  , _gen :: PureMT
  }

mke :: Word64 -> EventGen
mke = EventGen 0 Heap.empty Map.empty Map.empty . pureMT

makeLenses ''EventGen

useGen :: (MonadState EventGen m) => (PureMT -> (t, PureMT)) -> m t
useGen thing = do
  mt <-
    gets (view gen) :: (MonadState EventGen m) =>
                         m PureMT
  let (ws, newMt) = thing mt
  modify' (set gen newMt)
  return ws

$(monadRandom
    [d|
  instance Monad m => MonadRandom (StateT EventGen m) where
          getRandomWord64 = useGen randomWord64
          getRandomDouble = useGen randomDouble
  |])

mkEventGen :: (MonadReader Opt m) => Word64 -> m EventGen
mkEventGen seed = execStateT fn (mke seed)
  where
    fn ::
         (Monad m, MonadRandom (StateT EventGen m), MonadReader Opt m)
      => (StateT EventGen m) ()
    fn = foldM (\_ -> generateNewEvent 0.0) () gridIdxs

-- COMPILES, although you get:
--  Illegal instance declaration for
--    ‘MonadRandom (StateT EventGen m)’
--    (All instance types must be of the form (T a1 ... an)
--     where a1 ... an are *distinct type variables*,
--     and each type variable appears at most once in the instance head.
--     Use FlexibleInstances if you want to disable this.)
--  In the instance declaration for ‘MonadRandom (StateT EventGen m_a1cSc)’ (haskell-dante)
-- | Push an event into the event generator.
push ::
     (MonadState EventGen m)
  => Double
  -> EType
  -> Cell
  -> Maybe Ch
  -> Maybe Cell
  -> m ()
push time etype cell endCh hoffCell = do
  eId <- use egId
  let event = Event time etype cell endCh hoffCell
  modifyPart events $ Map.insert eId event
  forM_
    endCh
    (\ch -> do
       modifyPart queue $ Heap.insert EventKey {ekTime = time, ekId = eId}
       modifyPart endIds $ Map.insert (cell, ch) eId)
  return ()

-- | Retrieve the highest priority event from the event generator
pop :: (MonadState EventGen m) => m Event
pop
  -- Pop an identifier from the heap and retrieve the corresponding event
  -- from the hashmap. Then delete the it from the hashmaps.
 = do
  eKey <- statePart queue $ fromJust . Heap.view
  let eId = ekId eKey
  event <- (Map.! eId) <$> gets _events
  modifyPart events $ Map.delete eId
  forM_
    (view evEndCh event)
    (\ch -> modifyPart endIds (Map.delete (view evCell event, ch)))
  return event

-- liftState :: (MonadState s ms, MonadState t mt) => ALens' s t -> mt a -> ms a
-- liftState l m = state $ \s -> case runState m $ s ^# l of
--                                (a, b) -> (a, s & l #~ b)
-- | Using a lens, zoom in on a part of the state, apply the monadic action
-- and return the result.
statePart :: (MonadState s m) => Lens' s s' -> (s' -> (a, s')) -> m a
statePart lenss act = do
  s <- use lenss
  let (a, s') = act s
  modify' (set lenss s')
  return a

-- -- | Using a lens, zoom in on a part of the state, apply the monadic action
-- -- and return the result.
-- statePartM :: (MonadState s m) => Lens' s s' -> m (a, s') -> m ()
-- statePartM lenss act = do
--   modify' $ over lenss (map snd act)
--   return ()
-- | Using a lens, zoom in on a part of the state, apply the state transformer
modifyPart :: (MonadState s m) => Lens' s s' -> (s' -> s') -> m ()
modifyPart lenss mod = modify' (over lenss mod)

-- | Correct an event (and its keys) to reflect a channel reassignment
reassign :: Cell -> Ch -> Ch -> State EventGen ()
reassign cell fromCh toCh = do
  eId <-
    zoom endIds $ do
      eId <- state $ mapRemove (cell, fromCh)
      modify' $ Map.insert (cell, toCh) eId
      return eId
  zoom events . modify' $ Map.adjust (set evEndCh (Just toCh)) eId
  return ()

generateNewEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m)
  => Double
  -> Cell
  -> m ()
generateNewEvent time cell = do
  lam <- asks callRate
  dt <- sampleRVar (exponential (1 / lam :: Double))
  _ <- push (time + dt) NEW cell Nothing Nothing
  return ()

generateHoffNewEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m)
  => Double
  -> Cell
  -> Ch
  -> m ()
generateHoffNewEvent time cell ch = do
  lam <- asks callDurNew
  dt <- sampleRVar (exponential (lam :: Double))
  let neighs = getNeighs 2 cell False
  neigh_i <- sampleRVar $ uniform 0 (length neighs - 1)
  let toCell = neighs !! neigh_i
      t = time + dt
  -- A termination event (END) immediately succeeded
  -- by an arrival event (HOFF) in a neighboring cell
  _ <- push t END cell (Just ch) (Just toCell)
  _ <- push t HOFF toCell Nothing Nothing
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
  _ <- push (time + dt) END cell (Just ch) Nothing
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
