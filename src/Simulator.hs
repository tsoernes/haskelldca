{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Simulator where

import Base
import Control.Arrow ((&&&))
import Control.Lens (Lens', lens, makeLenses, over, set, use, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, Reader, asks)
import Control.Monad.State (MonadState, modify', runState)
import Data.Array.Accelerate (Exp)
import Data.Maybe (isNothing)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import EventGen (EventGen, mkEventGen, pop)
import Gridfuncs (featureRep, mkGrid)
import Opt
import Stats
  ( Stats
  , mkStats
  , statsEventAcceptNew
  , statsEventArrivalHoff
  , statsEventArrivalNew
  , statsEventEnd
  , statsEventRejectNew
  )

data SimState = SimState
  { _ssGrid :: Grid
  , _ssFrep :: Frep -- Strictly speaking, the frep is not a part of the simulator
  -- The next event for which an action must be selected and executed on 'grid'
  , _ssEvent :: Event
  , _ssEventgen :: EventGen
  , _ssStats :: Stats
  }

makeLenses ''SimState

mkSimState :: UTCTime -> Word64 -> Reader Opt SimState
mkSimState time seed = do
  eg <- mkEventGen seed
  let (event, eg') = runState pop eg :: (Event, EventGen)
  li <- asks Opt.logIter
  let g = mkGrid
      f = featureRep g
  return $ SimState g f event eg' (mkStats time li)

-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s); execute the action on the grid and return the resulting reward.
environmentStep ::
     (MonadReader Opt m, MonadState SimState m) => Maybe Ch -> m (Exp Int)
environmentStep act = do
  ev <- use ssEvent
  let eType = view evType ev
      time = view evTime ev :: Double
      cell = view evCell ev :: Cell
  -- Log call events to Stats record
  case eType of
    NEW -> do
      modify' $ over ssStats statsEventArrivalNew
      case act of
        Just _ -> modify' (over ssStats statsEventAcceptNew)
        Nothing -> modify' (over ssStats statsEventRejectNew)
    HOFF -> do
      modify' (over ssStats statsEventArrivalHoff)
      when (isNothing act) $ modify' (over ssStats statsEventRejectNew)
    END -> modify' (over ssStats statsEventEnd)
  -- Generate call events
  -- modify' $ over ssEventgen (\s -> (runStateT (generateNewEvent time cell) s) >>= put $ (snd) ==> get)
  -- case eType of
  --   NEW -> do
  --     modify' $ zoom ssEventgen (generateNewEvent time cell)
  --     forM_ act
  --       (\ch -> do
  --         -- p <- zoom _1 (state random :: StateT StdGen (Reader Opt) Double)
  --         p <- sampleRVar (stdUniform)
  --         ph <- asks hoffProb
  --         if p < ph
  --           then generateHoffNewEvent time cell ch
  --           else generateEndEvent time cell ch)
  --   HOFF -> forM_ act (generateHoffEndEvent time cell)
  --   END -> return ()
  -- Execute action, if any, on grid and return call count as reward
  -- forM_ act (zoom ssGrid . modify' . executeAction ev)
  -- boolSum2 <$> use ssGrid
  return undefined

pairLens2 :: Lens' s x -> Lens' s y -> Lens' s (x, y)
pairLens2 l1 l2 =
  lens (view l1 &&& view l2) (\s (x, y) -> set l1 x . set l2 y $ s)

pairLens3 :: Lens' s x -> Lens' s y -> Lens' s z -> Lens' s (x, y, z)
pairLens3 l1 l2 l3 =
  lens
    (\s -> (view l1 s, view l2 s, view l3 s))
    (\s (x, y, z) -> set l1 x . set l2 y . set l3 z $ s)
