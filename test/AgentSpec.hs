module AgentSpec where

import Agent
import Base
import Test.Hspec
import Gridfuncs
import Data.Array.Accelerate
import AccUtils
import TestBase
import Opt

spec :: Spec
spec = do
  let grid' = mkGrid
      setIdx _ = constant (Z :. 3 :. 3 :. 13) :: Exp DIM3
      grid = permute const grid' setIdx (unit $ lift True)
      frep = featureRep grid

  describe "forward" $ do
    let agent = mkAgent
        res = runExp (backend popts) $ forward frep agent
    it "Should not crash .." $ res `shouldBe` res
