module GridneighsSpec where

import qualified Data.Set
import Gridneighs
import Base
import Test.Hspec

spec :: Spec
spec =
  describe "periphery" $ do
    it "implementations are equal" $ do
      [(cell, p1 d cell) | cell <- gridIdxs, d <- [1..4]]
      `shouldBe`
      [(cell, p2 d cell) | cell <- gridIdxs, d <- [1..4]]
  where
    p1 d cell = Data.Set.fromList $ periphery d cell
    p2 d cell = Data.Set.fromList $ periphery' d cell
