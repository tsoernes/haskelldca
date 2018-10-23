module GridfuncsSpec where

import Simulator
import Base
import Test.Hspec
import Gridfuncs
import AccUtils
import Data.Array.Accelerate as A
import Control.Monad.Reader (runReader)
import Prelude as P
import TestBase
import Opt
import Control.Lens (view)

spec :: Spec
spec = do
  -- Remember notation: grid[row, col, channel] = channel_status (free/inuse),
  -- and remember that 'free' channel status does not imply that it's 'eligible'
  -- (see docs for `eligibleChs`).
  let grid = mkAGrid
  let bkend = backend popts

  describe "inuseChannels" $ do
    -- Assign grid[3, 3, 13] = 1
    -- That should be the only channel in use
    let ch = 13
        grid' = afterstate (3, 3) ch True grid
        actual = toList $ run bkend $ inuseChs (constant (3, 3)) grid'
        target = [ch]
    it "One ch in use" $ actual `shouldBe` target

  describe "eligibleChannels" $ do
    -- Assign grid[3, 2, 0] = 1
    -- Then grid[3, 3, 0] should not be eligible
    -- All other channels in that cell should be eligible
    let ch = 0
        setIdx _ = constant (Z :. 3 :. 2 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        cell = constant (3, 3)
        actual = toList $ run bkend $ eligibleChs cell grid'
        actual' = toList $ run bkend $ eligibleChs cell grid'
        target = [1..cHANNELS-1]
      in do it "One less eligible channel if neighbor uses that channel" $ actual `shouldBe` target
            it "Acc impl equals non-acc" $ actual `shouldBe` actual'

    let cell1 = constant (0, 1)
        cell2 = constant (5, 0)
        grid3b = afterstate' cell1 69 True grid
        grid3b' = afterstate' cell2 69 True grid3b
        elig1A = eligibleChs cell1 grid3b'
        (nElig1, elig1_) = runExpAcc bkend (size elig1A) elig1A
        elig1 = toList elig1_
        elig2 = toList $ run bkend $ eligibleChs cell2 grid3b'
      in do it "Same eligible channels" $ elig1 `shouldBe` elig2
            it "Should have 69 eligible channels" $ nElig1 `shouldBe` (cHANNELS - 1)

  describe "afterstates" $
    let grid' = afterstate (3, 3) 0 True grid
        chs = eligibleChs (constant (3, 3)) grid'
        -- Get the afterstates of a call arrival
        -- and calculate a (partial) frep for each outcome
        afsB = afterstates grid' (3, 3) NEW chs
        afsI = A.map boolToInt afsB :: Acc (Array DIM4 Int)
        s1 = run bkend $ A.sum $ flatten afsI
      in it ("can run w/o crashing / afsI sum:" P.++ show s1) $ s1 `shouldBe` s1

  describe "featureRep" $
    let grid' = afterstate (3, 3) 0 True grid
        frep = run bkend $ featureRep grid'
      in it "can run w/o crashing" $ frep `shouldBe` frep

  describe "featureRepCmp" $
    let (frep1a, frep1b) = run bkend $ lift (featureRep grid, featureRep' grid)
        grid2 = afterstate (3, 3) 0 True grid
        (frep2a, frep2b) = run bkend $ lift (featureRep grid2, featureRep' grid2)
        grid3__ = afterstate (3, 3) 1 True grid2
        grid3_ = afterstate (5, 0) 0 True grid3__
        grid3 = afterstate (5, 0) 1 True grid3_
        (frep3a, frep3b) = run bkend $ lift (featureRep grid3, featureRep' grid3)
      in do it "empty" $ frep1a `shouldBe` frep1b
            it "1 ch in use" $ frep2a `shouldBe` frep2b
            it "many chs in use" $ frep3a `shouldBe` frep3b

  describe "incAfterStateFreps " $ do
    let ch = 0
        setIdx _ = constant (Z :. 3 :. 3 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        cell = constant (3, 3)
        chs = eligibleChs cell grid'
        frep = featureRep grid'
        afreps = incAfterStateFreps cell (constant False) chs grid' frep
        rafreps = run bkend afreps
        afreps_sh = arrayShape rafreps
        n_chs = runExp bkend $ A.length chs
      in do it "can run w/o crashing due to nested data parallelism" $
              rafreps `shouldBe` rafreps
            it "has the right shape" $
              afreps_sh `shouldBe` (Z :. n_chs :. rOWS :. cOLS :. cHANNELS + 1)
    let (frep1a, frep1b) = run bkend $ lift (featureRep grid, featureRep' grid)
        -- TODO compare incremental with manual
        grid2 = afterstate (3, 3) 0 True grid
        (frep2a, frep2b) = run bkend $ lift (featureRep grid2, featureRep' grid2)
        grid3__ = afterstate (3, 3) 1 True grid2
        grid3_ = afterstate (5, 0) 0 True grid3__
        grid3 = afterstate (5, 0) 1 True grid3_
        (frep3a, frep3b) = run bkend $ lift (featureRep grid3, featureRep' grid3)
      in return ()

  describe "incAfterStateFrepsCmp " $
    let cell1 = (4, 5)
        cell2 = (6, 2)
        grid1 = afterstate cell1 69 True grid
        grid2 = afterstate cell2 49 True grid1
        grid3 = afterstate cell2 69 True grid2
        grid4 = afterstate cell2 69 False grid3

        frep0 = featureRep grid
        frep1a = featureRep grid1
        frep1b = featureRep' grid1
        frep1c = incAfterStateFrep cell1 False 69 grid frep0

        frep2a = featureRep grid2
        frep2b = featureRep' grid2
        frep2c = incAfterStateFrep cell2 False 49 grid1 frep1a

        frep3a = featureRep grid3
        frep3b = featureRep' grid3
        frep3c = incAfterStateFrep cell2 False 69 grid2 frep2a

        frep4a = featureRep grid4
        frep4b = featureRep' grid4
        frep4c = incAfterStateFrep cell2 True 69 grid3 frep3a
        whereDiff4c = indicesOf3 $ A.zipWith (A./=) frep4a frep4c
        -- [4,2,70], [5,1,70], [5,2,70], [6,0,70], [6,1,70], [6,2,70]
        -- A _subset_ of the d2-neighbors of `cell2` and the cell itself
        -- has not had their elig-chs increased on termination.
        -- In particular, it is the subset not covered by the d2 nb.hood of `cell1`

        (afrep1a, afrep1b, afrep1c,
         afrep2a, afrep2b, afrep2c,
         afrep3a, afrep3b, afrep3c,
         afrep4a, afrep4b, afrep4c, aWhereDiff4c)
          = run bkend $ lift
          (frep1a, frep1b, frep1c,
           frep2a, frep2b, frep2c,
           frep3a, frep3b, frep3c,
           frep4a, frep4b, frep4c, whereDiff4c)

      in do it "manual-new shouldBe manual-old #1" $
              afrep1b `shouldBe` afrep1a
            it "inc (base manual-old) shouldBe manual-old #1" $
              afrep1c `shouldBe` afrep1a

            it "manual-new shouldBe manual-old #2" $
              afrep2b `shouldBe` afrep2a
            it "inc (base manual-old) shouldBe manual-old #2" $
              afrep2c `shouldBe` afrep2a

            it "manual-new shouldBe manual-old #3" $
              afrep3b `shouldBe` afrep3a
            it "inc (base manual-old) shouldBe manual-old #3" $
              afrep3c `shouldBe` afrep3a

            it "manual-new shouldBe manual-old #4" $
              afrep4b `shouldBe` afrep4a
            it ("inc (base manual-old) shouldBe manual-old #4\n" P.++ showShapes aWhereDiff4c) $
              afrep4c `shouldBe` afrep4a

  describe "selectAction" $ do
    let sstate = runReader (mkSimState seed) popts
        ch = 0
        setIdx _ = constant (Z :. 3 :. 3 :. ch) :: Exp DIM3
        grid' = permute const grid setIdx (unit $ lift True)
        frep' = featureRep grid'
        cell = constant (3, 3)
        chs = eligibleChs cell grid'
        eIsEnd = constant False
        agent = use $ runReader mkAgent popts
        -- A3 _idx _qval _afrep
        result = selectAction cell eIsEnd chs agent grid' frep'
        (idx, qval, afrep) = run bkend result

    it "can run w/o crashing due to nested data parallelism" $ do
      -- this test does unnecessary work
      idx `shouldBe` idx
      qval `shouldBe` qval
      runExp bkend (the $ A.sum $ A.flatten $ A.use afrep) `shouldNotBe` 0

  describe "violatesReuseConstraint " $ do
    let res1 = runExp bkend $ violatesReuseConstraint grid
      in it "can run w/o crashing due to nested data parallelism" $
         res1 `shouldBe` False

    let grid2 = afterstate (3, 3) 3 True grid
        res2 = runExp bkend $ violatesReuseConstraint grid2
     in it "1 channel in use at whole grid does not violate" $
        res2 `shouldBe` False

    let grid3 = afterstate (3, 3) 3 True grid
        grid3' = afterstate (3, 3) 4 True grid3
        res3 = runExp bkend $ violatesReuseConstraint grid3'
      in it "2 adjacent channels in same cell does not violate" $
         res3 `shouldBe` False

    let grid4 = afterstate (3, 3) 3 True grid
        grid4' = afterstate (3, 4) 3 True grid4
        res4 = runExp bkend $ violatesReuseConstraint grid4'
      in it "Same channel neighboring cells (d=1) does violate" $
         res4 `shouldBe` True

    let grid5 = afterstate (3, 3) 3 True grid
        grid5' = afterstate (3, 5) 3 True grid5
        res5 = runExp bkend $ violatesReuseConstraint grid5'
      in it "Same channel neighboring cells (d=2) does violate" $
         res5 `shouldBe` True

    let grid6 = afterstate (3, 3) 3 True grid
        grid6' = afterstate (3, 6) 3 True grid6
        res6 = runExp bkend $ violatesReuseConstraint grid6'
      in it "Same channel neighboring cells (d=3) does not violate" $
         res6 `shouldBe` False

    let grid3b = afterstate (0, 1) 69 True grid
        grid3b' = afterstate (5, 0) 69 True grid3b
        res3b = runExp bkend $ violatesReuseConstraint grid3b'
      in it "Same channel in far away cells does not violate" $
         res3b `shouldBe` False
