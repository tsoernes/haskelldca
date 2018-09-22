{-# LANGUAGE FlexibleContexts #-}
{-# language NamedFieldPuns #-}
module EventGen(generateNewEvent, generateHoffNewEvent, generateEndEvent, generateHoffEndEvent, reassign) where

import           Base
import           Control.Monad.Reader (Reader, asks)
import           Control.Monad.State (State, StateT, MonadState, state)
import           Control.Monad.State.Lazy (State)
import           Data.Heap as Heap
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Random (sampleState, MonadRandom)
import           Data.Random.Lift (Lift)
import           Data.Functor.Identity (Identity)
import           Data.Random.Distribution.Exponential (exponential)
import           Data.Random.Distribution.Uniform (integralUniform)
import           Gridneighs (getNeighs)
import           Opt
import           System.Random (StdGen, mkStdGen, randomR)

data EventGen = EventGen { -- Last used event ID
                           egId :: EventId  
                           -- Min-heap of event-identifiers sorted on event times
                         , queue :: MinHeap EventKey
                           -- Mapping from event IDs to event structs
                         , events :: Map.Map EventId Event
                           -- Mapping from cell-channel pairs to END event IDs
                         , endIds :: Map.Map (Cell, Ch) EventId
                         }

mkEventGen :: EventGen
mkEventGen = EventGen { egId = 0, queue = empty, events = Map.empty, endIds = Map.empty }

push :: Double -> EType -> Cell -> Maybe Ch -> Maybe Cell -> EventGen -> EventGen
push time etype cell endCh hoffCell eg = eg { queue=queue', events=events', endIds=endIds', egId=id+1 }
  where
    id = egId eg
    event = Event { eId=egId eg, time, etype, cell, endCh, hoffCell }
    events' = Map.insert id event (events eg)
    (queue', endIds') = case endCh of
      Nothing -> (queue eg, endIds eg)
      Just ch -> (Heap.insert EventKey { ekTime=time, ekId=id} (queue eg)
                 ,Map.insert (cell, ch) id $ endIds eg)


-- | Retrieve the highest priority event from the event generator
pop :: EventGen -> (Event, EventGen)
pop eg = (event, eg { queue=queue', events=events', endIds=endIds' } )
  where
    -- Pop an identifier from the heap and retrieve the corresponding event
    -- from the hashmap. Then delete the it from the hashmaps.
    (eKey, queue') = fromJust $ view $ queue eg
    eId = ekId eKey
    event = events eg Map.! eId
    events' = Map.delete eId $ events eg
    endIds' = case endCh event of
      Nothing -> endIds eg
      Just ch -> Map.delete (cell event, ch) $ endIds eg

-- | Correct an event (and its keys) to reflect a channel reassignment
reassign :: Cell -> Ch -> Ch -> EventGen -> EventGen
reassign cell fromCh toCh eg = eg { endIds=endIds'' }
  where
    (id, endIds') = mapRemove (cell, fromCh) $ endIds eg
    endIds'' = Map.insert (cell, toCh) id endIds'
    events' = Map.adjust (\event -> event {endCh=Just toCh}) id $ events eg

generateNewEvent :: Double -> Cell -> EventGen -> StateT StdGen (Reader Opt) EventGen
generateNewEvent time cell eg = do
  lam <- asks callRate
  dt <- exponentialSt (1.0 / lam :: Double)
  return $ push (time+dt) NEW cell Nothing Nothing eg

generateHoffNewEvent :: Double -> Cell -> Ch -> EventGen -> StateT StdGen (Reader Opt) EventGen
generateHoffNewEvent time cell ch eg = do
  lam <- asks callDurNew
  dt <- exponentialSt (lam :: Double)
  let neighs = getNeighs 2 cell False
  neigh_i <- state $ randomR (0, length neighs - 1)
  let toCell = neighs !! neigh_i
      t = time + dt
      -- A termination event (END) immediately succeeded
      -- by an arrival event (HOFF) in a neighboring cell
      eg' = push t END cell (Just ch) (Just toCell) eg
      eg'' = push t HOFF toCell Nothing Nothing eg'
  return eg''

generateEndEvent :: Double -> Cell -> Ch -> EventGen -> StateT StdGen (Reader Opt) EventGen
generateEndEvent time cell ch eg = do
  lam <- asks callDurNew
  dt <- exponentialSt (lam :: Double)
  return $ push (time+dt) END cell (Just ch) Nothing eg

generateHoffEndEvent :: Double -> Cell -> Ch -> EventGen -> StateT StdGen (Reader Opt) EventGen
generateHoffEndEvent time cell ch eg = do
  lam <- asks callDurHoff
  dt <- exponentialSt (lam :: Double)
  return $ push (time+dt) END cell (Just ch) Nothing eg

-- | Look up the value for a key 'k'; remove and return the value
mapRemove :: Ord k => k -> Map.Map k a -> (a, Map.Map k a)
mapRemove k map = (map Map.! k, Map.delete k map)

-- | Given scale parameter 'lambda', sample a double from the exponential distribution
exponentialSt ::(MonadRandom (State s), MonadState s m) => Double -> m Double
exponentialSt = state . sampleState .exponential