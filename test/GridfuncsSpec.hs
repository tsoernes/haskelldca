module GridfuncsSpec where

import Simulator
import Base
import Test.Hspec
import Gridfuncs
import Gridneighs
import Control.Arrow ((***))
import AccUtils
import Data.Array.Accelerate as A
import Control.Monad.Reader (runReader)
import Prelude as P
import TestBase
import Opt
import Control.Lens (set, (^.), (.=))

spec :: Spec
spec = do
  -- Remember notation: grid[row, col, channel] = channel_status (free/inuse)
  -- .. and remember that 'free' does not mean 'eligible' (see docs for `eligibleChs`)
  let grid = mkGrid
  let bend = backend popts

  describe "inuseChannels" $ do
    -- Assign grid[3, 3, 13] = 1
    -- That should be the only channel in use
    let ch = 13
        setIdx _ = constant (Z :. 3 :. 3 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        actual = toList $ run bend $ inuseChs (3, 3) grid'
        target = [ch]
    it "One ch in use" $ actual `shouldBe` target

  describe "eligibleChannels" $ do
    -- Assign grid[3, 2, 0] = 1
    -- Then grid[3, 3, 0] should not be eligible
    -- All other channels in that cell should be eligible
    let ch = 0
        setIdx _ = constant (Z :. 3 :. 2 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        actual = toList $ run bend $ eligibleChs (3, 3) grid'
        target = [1..cHANNELS-1]
    it "One less eligible channel if neighbor uses that channel" $ actual `shouldBe` target

  describe "afterstates" $ do
    let setIdx _ = constant (Z :. 3 :. 3 :. 0) :: Exp DIM3
        grid' = permute const mkGrid setIdx (unit $ A.lift True)
        chs = eligibleChs (3, 3) grid'
        -- Get the afterstates of a call arrival
        -- and calculate a (partial) frep for each outcome
        afsB = afterstates grid' (3, 3) NEW chs
        afsI = A.map boolToInt afsB :: Acc (Array DIM4 Int)
        s1 = run bend $ A.sum $ flatten afsI
    it ("can run bend w/o crashing / afsI sum:" P.++ show s1) $ s1 `shouldBe` s1

  describe "featureRep" $ do
    let setIdx _ = constant (Z :. 3 :. 3 :. 0) :: Exp DIM3
        grid' = permute const mkGrid setIdx (unit $ A.lift True)
        frep = run bend $ featureRep grid'
    it "can run bend w/o crashing" $ frep `shouldBe` frep

  describe "incAfterStateFreps " $ do
    let ch = 0
        setIdx _ = constant (Z :. 3 :. 3 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        cell = (3, 3)
        chs = eligibleChs cell grid'
        frep = featureRep grid'
        afreps = incAfterStateFreps grid' frep cell NEW chs
        rafreps = run bend afreps
        afreps_sh = arrayShape rafreps
        n_chs = runExp bend $ A.length chs
    it "can run bend w/o crashing due to nested data parallelism" $
      rafreps `shouldBe` rafreps
    it "has the right shape" $
      afreps_sh `shouldBe` (Z :. n_chs :. rOWS :. cOLS :. cHANNELS + 1)

  describe "selectAction" $ do
    let sstate = runReader (mkSimState seed) popts
        ch = 0
        setIdx _ = constant (Z :. 3 :. 3 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        cell = (3, 3)
        chs = eligibleChs cell grid'
        ev = Event 1.0 NEW cell
        sstate' = set ssGrid grid' sstate
        sstate'' = set ssFrep (featureRep grid') sstate'
        sstate''' = set ssEvent ev sstate''
        (_idx, _qval, _afrep) = selectAction sstate''' chs
        (idx, qval, afrep) = (runExp bend _idx, runExp bend _qval, run bend _afrep)

    it "can run bend w/o crashing due to nested data parallelism" $ do
      idx `shouldBe` idx
      qval `shouldBe` qval
      afrep `shouldBe` afrep