module Main where

import Base
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Word (Word64)
import Gridfuncs
import Gridneighs
import Opt
import Options.Applicative
import Scratch
import Simulator
import LensUtils

import Data.Array.Accelerate (Elt, Scalar, Exp, Acc, Vector, Matrix, Array, Slice, Shape, DIM0, DIM1, DIM2, DIM3, DIM4, Z(..), (:.)(..), constant, index1, index2, index3, unindex1, unindex2, unindex3, (:.)(..), All(..), Z(..), arrayShape, arraySize, Exp, slice, boolToInt, the, unlift)
import qualified Data.Array.Accelerate as A
import Prelude as P

main :: IO ()
main = do
  let opts =
        info
          (getOpts <**> helper)
          (fullDesc <>
           header "DCA - Dynamic Channel Allocation by Reinforcement Learning")
  popts <- execParser opts
  print popts
  let seed = 0 :: Word64
  runSim seed popts
  return ()

-- main :: IO ()
-- main = do
--   let arrB = A.fill (index3 7 7 9) (A.lift False) :: Acc (Array DIM3 Bool)
--       x = run arrB
--   print x
--   return ()
