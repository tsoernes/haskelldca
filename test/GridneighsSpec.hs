module GridneighsSpec where

import qualified Data.Set
import Gridneighs
import Base
import Data.Foldable (for_)
import Test.Hspec

spec :: Spec
spec =
  describe "periphery" $ do
    for_ [1..4] $ \d ->
      for_ gridIdxs $ \cell ->
        it ("implementations are equal for distance " ++ show d ++ " at cell " ++ show cell) $ do
          p1 d cell `shouldBe` p2 d cell
      where
    p1 d cell = Data.Set.fromList $ periphery d cell
    p2 d cell = Data.Set.fromList $ periphery' d cell

