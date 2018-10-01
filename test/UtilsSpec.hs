{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module UtilsSpec where

import qualified Data.Set
import Utils
import Stats
import Data.Functor.Identity
import Base
import Data.Foldable (for_)
import Test.Hspec
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
  , get
  , put
  , gets
  , modify'
  , runStateT
  , runState
  )

data SomeState = SomeState { _x :: Int, _y :: [Int]}
makeLenses ''SomeState

fn :: Int -> (String, Int)
fn i = (show i, i + 1)

fnM :: (MonadState Int m) => m String
fnM = do
  i <- get
  put (i + 1)
  return $ show i

fnNoAct :: Int -> Int
fnNoAct i = i + 1

data Wrap = Wrap { _stats :: Stats }
makeLenses ''Wrap

-- | Modifying the state of a field in a state component
spec :: Spec
spec = do
  describe "statePart" $ do
    let ss = SomeState 3 []
        (a, ss') = runState (statePart x fn) ss
    it "running a (s -> (a, s)) state transition with a result" $ do
          a `shouldBe` "3"
          view x ss' `shouldBe` 4
          view y ss' `shouldBe` []

  describe "statePartM" $ do
    let ss = SomeState 2 [2]
        (a, ss') = runState (statePartM x fnM) ss
    it "running a (MonadState s m => m a) state transition with a result" $ do
          a `shouldBe` "2"
          view x ss' `shouldBe` 3
          view y ss' `shouldBe` [2]

    let wstats = Wrap mkStats
        (a, wstats') = runState (statePartM stats statsEventArrivalNew) wstats
    it "running a (MonadState s m => m a) state transition using lenses to modify nested state" $ do
          a `shouldBe` ()
          view (stats . nCurrArrivalsNew) wstats `shouldBe` 0
          view (stats . nCurrArrivalsNew) wstats' `shouldBe` 1

  describe "modifyPart" $ do
    let ss = SomeState (-1) [2, 3]
        (a, ss') = runState (modifyPart x fnNoAct) ss
    it "running a (s -> s) state transition without a result" $ do
          a `shouldBe` ()
          view x ss' `shouldBe` 0
          view y ss' `shouldBe` [2, 3]
