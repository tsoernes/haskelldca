{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
module Simulator where

import Stats
import Gridneighs
import Gridfuncs (boolSum2, executeAction, mkGrid, featureRep)
import EventGen
import Data.Array.Accelerate (Exp)
import Control.Monad.State (State, StateT, state, modify', runStateT, runState)
import Control.Monad.Reader (Reader, asks)
import Control.Monad (when, forM_, forM)
import Data.Maybe (isNothing)
import System.Random (StdGen, random)
import Control.Arrow ((***), (&&&))
import Base
import Opt
import Control.Lens (makeLenses, use, zoom, alongside, Lens', lens, view, set, _1, _2)
import Data.Time.Clock (UTCTime)

data SimState = SimState
  {
    _ssGrid :: Grid
  , _ssFrep :: Frep -- Strictly speaking, the frep is not a part of the simulator
  -- The next event for which an action must be selected and executed on 'grid'
  , _ssEvent :: Event
  , _ssEventgen :: EventGen
  , _ssStats :: Stats
  , _ssGen :: StdGen
  }

makeLenses ''SimState

mkSimState :: UTCTime -> StdGen -> Reader Opt SimState
mkSimState time gen = do
  (eg, gen') <- runStateT mkEventGen gen
  let (event, eg') = runState pop eg :: (Event, EventGen)
  li <- asks Opt.logIter
  let g = mkGrid
      f = featureRep g
  return $ SimState g f event eg' (mkStats time li) gen'


-- | Advance the environment 1 step: Log statistics for the event and its corresponding action;
-- | generate the new event(s); execute the action on the grid and return the resulting reward.
environmentStep :: Maybe Ch -> StateT SimState (Reader Opt) (Exp Int)
environmentStep act = do
  ev <- use ssEvent
  let eType = view evType ev
      time = view evTime ev
      cell = view evCell ev
  -- Log call events to Stats record
  zoom ssStats $ case eType of
    NEW -> do
      modify' statsEventArrivalNew
      case act of
        Just ch -> modify' statsEventAcceptNew
        Nothing -> modify' statsEventRejectNew
    HOFF -> do
      modify' statsEventArrivalHoff
      when (isNothing act) $ modify' statsEventRejectNew
    END -> modify' statsEventEnd
  -- Generate call events
  zoom (pairLens2 ssGen ssEventgen) $ case eType of
    NEW -> do
      generateNewEvent time cell
      forM_ act
        (\ch -> do
          p <- zoom _1 (state random :: StateT StdGen (Reader Opt) Double)
          ph <- asks hoffProb
          if p < ph
            then generateHoffNewEvent time cell ch
            else generateEndEvent time cell ch)
    HOFF -> forM_ act (generateHoffEndEvent time cell)
    END -> return ()
  -- Execute action, if any, on grid and return call count as reward
  forM_ act (zoom ssGrid . modify' . executeAction ev)
  boolSum2 <$> use ssGrid


pairLens2 :: Lens' s x -> Lens' s y -> Lens' s (x,y)
pairLens2 l1 l2 =
  lens (view l1 &&& view l2)
       (\s (x,y) -> set l1 x . set l2 y $ s)

pairLens3 :: Lens' s x -> Lens' s y -> Lens' s z -> Lens' s (x,y,z)
pairLens3 l1 l2 l3 =
  lens (\s -> (view l1 s, view l2 s, view l3 s))
       (\s (x,y,z) -> set l1 x . set l2 y . set l3 z $ s)
