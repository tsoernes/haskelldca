{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module EventGen
  ( EventGen(..)
  , mkEventGen
  , _mkEventGen
  , generateNewEvent
  , generateHoffNewEvent
  , generateEndEvent
  , generateHoffEndEvent
  , reassign
  , pop
  , egId
  , egEvents
  , egGen
  ) where

import EventGen.Internal
import Base
import Control.Lens ( (^.), uses, view, set )
import Control.Monad.Reader ( MonadReader, asks )
import Control.Monad.State.Lazy ( MonadState(state), StateT, execStateT, modify' )
import qualified Data.Heap as Heap ( view )
import qualified Data.Map.Strict as Map ( adjust, (!?), delete, insert )
import Data.Maybe ( fromJust )
import Data.RVar ( sampleRVar )
import Data.Random ( MonadRandom, uniform )
import Data.Random.Distribution.Exponential ( exponential )
import Data.Word ( Word64 )
import Gridneighs ( getNeighs )
import LensUtils ( modifyPart, statePartM, statePart  )
import Opt ( Opt(callDurHoff, callDurNew, callRate) )


-- | Retrieve the highest priority event from the event generator.
-- | Throws an error if the queue is empty.
pop :: (MonadState EventGen m) => m Event
pop = do
  -- queueIsNull <- uses egQueue null
  -- Pass queue through trace OP
  -- let q = trace ("Queue is empty before pop: " ++ show queueIsNull) queue
  eKey <- statePart egQueue $ fromJust . Heap.view
  -- ^ Pop an identifier from the heap and retrieve the corresponding event
  -- from the hashmap. Then delete the it from the hashmaps.
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

-- | Given a random seed, create an EventGen with initial events:
-- | one event queued for each cell.
mkEventGen :: (MonadReader Opt m) => Word64 -> m EventGen
mkEventGen seed = execStateT fn (_mkEventGen seed)
  where
    fn :: (MonadRandom (StateT EventGen m), MonadReader Opt m) => (StateT EventGen m) ()
    fn = mapM_ (generateNewEvent 0.0) gridIdxs


-- | Correct an event (and its keys) to reflect a channel reassignment
reassign :: (MonadState EventGen m) => Cell -> Ch -> Ch -> m ()
reassign cell fromCh toCh = do
  eId <-
    statePartM egEndIds $ do
      eId <- state $ mapRemove (cell, fromCh)
      modify' $ Map.insert (cell, toCh) eId
      return eId
  modifyPart egEvents $ Map.adjust (\ev -> set evType (END toCh (hoffCell $ view evType ev)) ev) eId
  return ()

generateNewEvent ::
     (MonadRandom m, MonadReader Opt m, MonadState EventGen m) => Double -> Cell -> m ()
generateNewEvent time cell = do
  lam <- asks callRate
  dt <- sampleRVar (exponential (lam / 60.0))
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
  dt <- sampleRVar (exponential (1.0 / lam :: Double))
  let neighs = getNeighs 2 cell False
  neigh_i <- sampleRVar $ uniform 0 (length neighs - 1)
  let toCell = neighs !! neigh_i
      t = time + dt
  _ <- push $ Event t (END ch (Just toCell)) cell
  _ <- push $ Event t HOFF toCell
  return ()

generateEndEvent  :: (MonadRandom m, MonadReader Opt m, MonadState EventGen m) => Double -> Cell -> Ch -> m ()
generateEndEvent time cell ch = asks callDurNew >>= _generateEndEvent time cell ch

generateHoffEndEvent ::
     (MonadState EventGen m, MonadReader Opt m, MonadRandom m) => Double -> Cell -> Ch -> m ()
generateHoffEndEvent time cell ch = asks callDurHoff >>= _generateEndEvent time cell ch
