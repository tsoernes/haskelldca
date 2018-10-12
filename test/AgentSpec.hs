module AgentSpec where

import Agent
import Base
import Test.Hspec
import Gridfuncs
import Data.Array.Accelerate
import AccUtils
import TestBase
import Opt
import Simulator
import Control.Monad.Reader (runReader)

spec :: Spec
spec = do
  let grid' = mkAGrid
      setIdx _ = constant (Z :. 3 :. 3 :. 13) :: Exp DIM3
      grid = permute const grid' setIdx (unit $ lift True)
      frep = featureRep grid

  describe "forward" $ do
    let (net, _, _) = runReader mkAgent popts
        res = runExp (backend popts) $ forward (prepFrep frep) (use net)
    it "Should not crash .." $ res `shouldBe` res
