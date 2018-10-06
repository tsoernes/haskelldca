-- See 'test/LensUtilsSpec' for examples of how these zooming operators (statePart/statePartM/modifyPart)
-- can be used

{-# LANGUAGE Rank2Types #-}

module LensUtils where

import Control.Arrow ((&&&))
import Control.Lens
  ( ALens'
  , Lens'
  , ( #%~ )
  , ( #~ )
  , (^#)
  , lens
  , makeLenses
  , over
  , set
  , use
  , view
  )
import Control.Monad.State.Lazy
  ( MonadState(state)
  , StateT
  , gets
  , modify'
  , runStateT
  )
import qualified Data.Heap as Heap
import qualified Data.Map.Strict as Map

-- | Using a lens, zoom in on a part of the state, apply the state function
-- and return the result.
statePart :: (MonadState s m) => ALens' s t -> (t -> (a, t)) -> m a
statePart lenss act = do
  -- Pull out part of state
  t <- gets (^# lenss)
  let (a, t') = act t
  modify' (lenss #~ t')
  return a

-- | Using a lens, zoom in on a part of the state, apply the monadic action
-- and return the result.
statePartM :: (MonadState s m) => ALens' s t -> StateT t m a -> m a
statePartM lenss act = do
  t <- gets (^# lenss)
  (a, t') <- runStateT act t
  modify' $ lenss #~ t'
  return a

-- | Using a lens, zoom in on a part of the state, apply the state transformer
modifyPart :: (MonadState s m) => ALens' s t -> (t -> t) -> m ()
modifyPart lenss mod = modify' (lenss #%~ mod)

pairLens2 :: Lens' s x -> Lens' s y -> Lens' s (x, y)
pairLens2 l1 l2 =
  lens (view l1 &&& view l2) (\s (x, y) -> set l1 x . set l2 y $ s)

pairLens3 :: Lens' s x -> Lens' s y -> Lens' s z -> Lens' s (x, y, z)
pairLens3 l1 l2 l3 =
  lens
    (\s -> (view l1 s, view l2 s, view l3 s))
    (\s (x, y, z) -> set l1 x . set l2 y . set l3 z $ s)
