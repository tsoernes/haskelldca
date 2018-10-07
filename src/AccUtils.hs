{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module AccUtils where

import Base ( Backend(..) )

import qualified Prelude as P ()
import Control.Arrow ( Arrow(first) )

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as Interp ( run )
import qualified Data.Array.Accelerate.LLVM.Native as CPU ( run )


-- | Execute Accelerate expressions
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
run CPU = CPU.run

runExp :: (Elt a) => Backend -> Exp a -> a
runExp backend expa = A.indexArray (run backend $ unit expa) Z

boolSum :: Shape sh => Acc (Array (sh :. Int) Bool) -> Exp Int
boolSum = the . sum . flatten . map boolToInt

-- | Create a rank-4 index from four Exp Int`s
index4 ::
     (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp i
  -> Exp i
  -> Exp i
  -> Exp i
  -> Exp (Z :. i :. i :. i :. i)
index4 k j i l = lift (Z :. k :. j :. i :. l)

unindex4 ::
     (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp (Z :. i :. i :. i :. i)
  -> (Exp i, Exp i, Exp i, Exp i)
unindex4 sh = (i, j, k, l)
  where
    Z :. i :. j :. k :. l = unlift sh

-- | Combined argmax and max:
-- | Return (idx :: sh, val) pair for the largest element in the array
argpmax :: (Ord a, Elt a, Shape sh) => Acc (Array sh a) -> (Exp sh, Exp a)
argpmax arr = unlift $ the x
  where
    x = fold1All fn (flatten $ indexed arr)
    fn :: (Elt sh, Ord e, Elt e) => Exp (sh, e) -> Exp (sh, e) -> Exp (sh, e)
    fn a b = cond (snd a > snd b) a b

-- | Combined argmax and max:
-- | Return (idx :: Int, val) pair for the largest element in the vector
argpmax1 :: (Ord a, Elt a) => Acc (Array DIM1 a) -> (Exp Int, Exp a)
argpmax1 = first unindex1 . argpmax

-- Different vector-vector and vector-matrix multiplications follow.
-- I don't know which one to use. Perhaps none and instead use BLAS bindings

vvMul :: Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
vvMul xs ys = fold (+) 0 (zipWith (*) xs ys)

-- mvMul :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- mvMul mat vec =
--   let Z :. rows :. _ = unlift (shape mat) :: Z :. Exp Int :. Exp Int
--       vec' = replicate (lift (Z :. rows :. All)) vec
--    in fold (+) 0 (zipWith (*) mat vec')

-- mmMul :: Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
-- mmMul mat1 mat2 = undefined

-- outer :: (Num a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Matrix a)
-- outer x y =
--   zipWith
--     (*)
--     (replicate (lift $ Any :. All :. length y) x)
--     (replicate (lift $ Any :. length x :. All) y)

-- multiplyMatrixVector :: (Num a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- multiplyMatrixVector m v =
--   let rows = fst . unindex2 $ shape m :: Exp Int
--    in fold1 (+) $ zipWith (*) m (replicate (lift $ Any :. rows :. All) v)

-- multiplyMatrixMatrix :: (Num a) => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
-- multiplyMatrixMatrix x y =
--   let rows = fst . unindex2 $ shape x :: Exp Int
--       cols = snd . unindex2 $ shape y :: Exp Int
--       a1 = replicate (lift $ Z :. All :. All :. cols) x -- :: Acc (Array DIM3 a)
--       a2 = replicate (lift $ Z :. rows :. All :. All) y -- :: Acc (Array DIM3 _)
--       b = zipWith (*) a1 a2 -- :: Acc (Array DIM3 _)
--       -- c = transpose b
--   -- in fold1 (+) $ c
--    in undefined

-- (#*#) :: (Num a) => Acc (Array DIM2 a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
-- v #*# w =
--   let (k, m) = unlift . unindex2 $ shape v :: (Exp Int, Exp Int)
--       (m', n) = unlift . unindex2 $ shape w :: (Exp Int, Exp Int)
--    in generate (index2 k n) (aux v w)
--   where
--     aux :: (Num e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Exp DIM2 -> Exp e
--     aux v w sh =
--       let (i, j) = unlift $ unindex2 sh :: (Exp Int, Exp Int)
--           v' = slice v (lift $ Z :. i :. All)
--           w' = slice w (lift $ Z :. All :. j)
--        in the $ sum $ zipWith (*) v' w'

