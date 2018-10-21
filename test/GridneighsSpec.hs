module GridneighsSpec where

import Base
import Control.Arrow ((***))
import Data.Array.Accelerate
  ( (:.)(..)
  , (:.)(..)
  , Acc
  , All(..)
  , Array
  , DIM0
  , DIM1
  , DIM2
  , DIM3
  , DIM4
  , Elt
  , Exp
  , Matrix
  , Scalar
  , Shape
  , Slice
  , Vector
  , Z(..)
  , Z(..)
  , arrayShape
  , arraySize
  , boolToInt
  , constant
  , index1
  , index2
  , index3
  , permute
  , slice
  , the
  , unindex1
  , unindex2
  , unindex3
  , unit
  , unlift
  )
import qualified Data.Array.Accelerate as A
import Text.Printf (printf)
import AccUtils
import TestBase

import Data.Foldable (forM_, for_)
import qualified Data.Set
import Gridneighs
import Test.Hspec
import Opt

spec :: Spec
spec = do
  let be = backend popts

  describe "neighborhood-wwo-self" $
    forM_ [1 .. 4] $ \d ->
      forM_ gridIdxs $ \cell' -> do
        let cell = (A.lift *** A.lift) cell'
            neighs4a = getNeighborhoodAcc d cell False :: Acc (Array DIM1 Cell)
            neighs4b = getNeighborhoodAcc d cell True :: Acc (Array DIM1 Cell)
        it
          (printf
             "The focal cell comes first in the neighborhood, otherwise they are equal. Focal: %v, dist %d"
             (show cell')
             d) $
          run be neighs4a `shouldBe` run be (A.tail neighs4b)
