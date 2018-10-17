module EventGenSpec where

import qualified Data.Set as Set
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import EventGen
import EventGen.Internal
import Base
import Data.Foldable (for_)
import Test.Hspec
import Data.Word ( Word64 )
import Opt
import Control.Lens
import Control.Monad.Reader ( runReader, liftIO  )
import Control.Monad.State.Strict ( runState, runStateT, evalStateT, execStateT)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import TestBase
import qualified Data.Heap as Heap (null)
import qualified Data.Map.Strict as Map (null)

-- | For debugging only: Check that the Heap and the two Maps are all empty
isEmpty :: EventGen -> Bool
isEmpty (EventGen _ q evs eids _) = Heap.null q && Map.null evs && Map.null eids

spec :: Spec
spec = do
  -- THESE FIRST TESTS TEST THE INTERNAL IMPLEMENTATION DETAILS OF THE EVENTGEN
  describe "Pushing a NEW event to an empty EventGen" $ do
    let eg = _mkEventGen seed
        ev = Event {_evTime=1.3, _evType=NEW, _evCell=(1,2)}
        eg' = runReader (execStateT (push ev) eg) popts
        egid = view egId eg' :: Int
        ev' = (Map.!?) (view egEvents eg') 0
    it "EventGen ID should be 1" $ egid `shouldBe` 1
    it "Map:event should not be empty" $ isJust ev' `shouldBe` True
    it "Map:event should contain the inserted event" $ fromJust ev' `shouldBe` ev

  -- describe "Pushing an END event to an empty EventGen" $ do
  --   let eg = _mkEventGen seed
  --       ev = Event {_evTime=1.3, _evType=NEW, _evCell=(1,2), _evEndCh=Nothing, _evHoffCell=Nothing}
  --       eg' = runReader (execStateT (push ev) eg) popts
  --       eid = view egId eg' :: Int
  --       ev' = (Map.!?) (view egEvents eg') eid
  --   it "It's ID should be 0" $ eid `shouldBe` 0
  --   it "Map:event should not be empty" $ isJust ev' `shouldBe` True
  --   it "Map:event should contain the inserted event" $ fromJust ev' `shouldBe` ev

  --- THE TESTS BELOW DOES NOT TEST IMPLEMENTATION DETAILS
  describe "An empty EventGen" $ do
    let eg = _mkEventGen seed

    it "Pushing an event to an empty generator then popping should return the same event" $ do
      let ev = Event {_evTime=1.3, _evType=NEW, _evCell=(1,2)}
          (ev', eg') = runReader (runStateT (push ev >> pop) eg) popts
      ev `shouldBe` ev'

    it "Generating a NEW event then popping it" $ do
      let eTime = 1.3
          eCell = (1,2)
          (ev, eg') = runReader (runStateT (generateNewEvent eTime eCell >> pop) eg) popts
      view evCell ev `shouldBe` eCell
      view evType ev `shouldBe` NEW
      isEmpty eg' `shouldBe` True

    -- TODO
    -- it "Handing off a call produces the correct two events" $ do
    --   let eTime = 1.3
    --       eCell = (1,2)
    --       (ev, eg') = runReader (runStateT (generateNewEvent eTime eCell >> pop) eg) popts
    --   view evTime ev `shouldBe` eTime
    --   view evCell ev `shouldBe` eCell
    --   view evType ev `shouldBe` NEW
    --   isEmpty eg' `shouldBe` True

  describe "An EventGen filled with initial events" $ do
    let eg = runReader (mkEventGen seed) popts

    it "Should be able to pop an event from a fresh generator without running empty" $ do
      let (ev, eg') = runState pop eg
      ev `shouldBe` ev

    it "Should be exactly RxC events in a fresh generator " $ do
      -- Pop RxC events
      let (evs, eg') = runState (mapM (const pop) gridIdxs) eg
      evs `shouldBe` evs
      _egId eg' `shouldBe` _egId eg'
      -- Pop one more
      let (evs', eg'') = runState pop eg'
      evaluate (eg'') `shouldThrow` anyErrorCall

  describe "Reassigning in EventGen" $ do
    let eg = _mkEventGen seed
        eTime = 1.3
        eCell = (1, 2)
        fromCh = 7
        toCh = 13
        ev = Event {_evTime=eTime, _evType=(END fromCh Nothing), _evCell=eCell}
        (ev', eg') = runReader (runStateT (push ev >> reassign eCell fromCh toCh >> pop) eg) popts
    it "should have changed termination channel (endCh) but nothing else" $ do
      -- Channel in use should have changed:
      -- Nothing else should have changed:
      view evTime ev' `shouldBe` eTime
      view evCell ev' `shouldBe` eCell
      view evType ev' `shouldBe` (END toCh Nothing)
      -- Should verify that eg' is empty
      isEmpty eg' `shouldBe` True

