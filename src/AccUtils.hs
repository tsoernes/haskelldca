{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module AccUtils where

import Base

import Control.Arrow (Arrow(first))
import qualified Prelude as P
import qualified Data.List as L
import Prelude (Show, show)

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Array.Sugar (shapeToList)
import qualified Data.Array.Accelerate.Interpreter as Interp (run, run1, runN)
import qualified Data.Array.Accelerate.LLVM.Native as CPU (run, run1, runN)
import Data.Array.Accelerate.Trafo


pattern T2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a, b)
pattern T2 a b <- (unlift -> (a, b))
  where T2 a b = lift (a, b)
{-# COMPLETE T2 #-}


pattern T3 :: (Elt a, Elt b, Elt c) => Exp a -> Exp b -> Exp c -> Exp (a, b, c)
pattern T3 a b c <- (unlift -> (a, b, c))
  where T3 a b c = lift (a, b, c)
{-# COMPLETE T3 #-}


pattern A0 :: (Elt a) => Exp a -> Acc (Scalar a)
pattern A0 a <- (the -> a)
  where A0 = unit
{-# COMPLETE A0 #-}


pattern A2 :: (Arrays a, Arrays b) => Acc a -> Acc b -> Acc (a, b)
pattern A2 a b <- (unlift -> (a, b))
  where A2 a b = lift (a, b)
{-# COMPLETE A2 #-}


pattern A3 :: (Arrays a, Arrays b, Arrays c) =>
        Acc a -> Acc b -> Acc c -> Acc (a, b, c)
pattern A3 a b c <- (unlift -> (a, b, c))
  where A3 a b c = lift (a, b, c)
{-# COMPLETE A3 #-}


pattern D2 :: Exp Int -> Exp Int -> Exp DIM2
pattern D2 a b <- (unlift . unindex2 -> (a, b))
  where D2 a b = index2 a b
{-# COMPLETE D2 #-}


pattern D3 :: Exp Int -> Exp Int -> Exp Int -> Exp DIM3
pattern D3 a b c <- (unlift . unindex3 -> (a, b, c))
  where D3 a b c = index3 a b c
{-# COMPLETE D3 #-}


pattern D4 :: Exp Int -> Exp Int -> Exp Int -> Exp Int -> Exp DIM4
pattern D4 a b c d <- (unlift . unindex4 -> (a, b, c, d))
  where D4 a b c d = index4 a b c d
{-# COMPLETE D4 #-}


-- | Execute Accelerate expressions
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
run CPU = CPU.run


-- | Execute Accelerate expressions
runN :: Afunction f => Backend -> f -> AfunctionR f
runN Interpreter = Interp.runN
runN CPU = CPU.runN


-- | Execute Accelerate expressions
run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter = Interp.run1
run1 CPU = CPU.run1


-- | Run just once ..?
recycle1E :: (Elt a, Arrays b) => (Exp a -> Acc b) -> a -> Acc b
recycle1E fn inp = use $ run1 CPU (fn . the) (scalar inp)

-- -- | Run just once ..?
-- recycleConst :: forall a. (Arrays a) => Acc a -> Acc a
-- recycleConst acc = use a
--   where
--     acc' :: Acc (Scalar ()) -> Acc a
--     acc' _ = acc
--     inp = scalar ()
--     f = $( CPU.runQ acc' )
--     a = f inp


-- | Execute Accelerate expressions
run1S ::
     forall a. (Arrays a)
  => Backend
  -> Acc a
  -> a
run1S bkend acc = run1 bkend acc' $ scalar ()
  where
    acc' :: Acc (Scalar ()) -> Acc a
    acc' _ = acc


runExp :: (Elt a) => Backend -> Exp a -> a
runExp backend expa = A.indexArray (run backend $ unit expa) Z


-- | Run an Exp and an Acc
runExpAcc ::
     forall a b. (Elt a, Arrays b)
  => Backend
  -> Exp a
  -> Acc b
  -> (a, b)
runExpAcc backend expA accB = (eltA, arrB)
  where
    accA = unit expA :: Acc (Array DIM0 a)
    acc = A.lift (accA, accB) :: Acc (Array DIM0 a, b)
    (arrA, arrB) = run backend acc
    eltA = A.indexArray arrA Z


-- | Run a Scalar and an Acc
runScalarAcc ::
     forall a b. (Elt a, Arrays b)
  => Backend
  -> Acc (Array DIM0 a, b)
  -> (a, b)
runScalarAcc backend acc = (theR arrA, arrB)
  where
    (arrA, arrB) = run backend acc


-- | Like `the`, but for non-Acc Scalars.
theR :: (Elt e) => Scalar e -> e
theR arr = A.indexArray arr Z


-- | Like `unit`
scalar :: (Elt e) => e -> Scalar e
scalar e = A.fromList Z [e]


boolSum :: Shape sh => Acc (Array (sh :. Int) Bool) -> Exp Int
boolSum = the . sum . flatten . map boolToInt


arrEq ::
     forall sh e. (Shape sh, Elt e, Eq e)
  => Acc (Array sh e)
  -> Acc (Array sh e)
  -> Exp Bool
arrEq arr1 arr2 = the $ all P.id (flatten eqs)
  where
    eqs = zipWith (==) arr1 arr2 :: Acc (Array sh Bool)


-- | Create a rank-4 index from four Exp Int`s
index4 ::
     (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp i
  -> Exp i
  -> Exp i
  -> Exp i
  -> Exp (Z :. i :. i :. i :. i)
index4 k j i l = lift (Z :. k :. j :. i :. l)


-- | Destruct a rank-4 index into an Exp tuple of Int`s
unindex4 ::
     forall i. (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp (Z :. i :. i :. i :. i)
  -> Exp (i, i, i, i)
unindex4 sh = lift (i, j, k, l)
  where
    Z :. i :. j :. k :. l = unlift sh :: Z :. Exp i :. Exp i :. Exp i :. Exp i


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


-- Perhaps use accelerate BLAS bindings instead?
-- | Vector-vector multiplication
vvMul :: Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
vvMul xs ys = fold (+) 0 (zipWith (*) xs ys)


-- | Matrix-vector multiplication
mvMul :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvMul mat vec =
  let D2 rows _ = shape mat
      vec' = replicate (lift (Z :. rows :. All)) vec
   in fold (+) 0 (zipWith (*) mat vec')


-- | One-hot array of eligible channels
-- | Convert a one-hot vector (sparse repr) to a vector of indecies (dense repr)
indicesOf :: Acc (Array DIM1 Bool) -> Acc (Array DIM1 Int)
indicesOf arr = iHot
  where
    seArr = indexed arr :: Acc (Array DIM1 (DIM1, Bool))
    -- Filter out (index, elem) pairs where elem is not True,
    -- and keep only the indices (as ints)
    shHot = afst $ filter snd seArr
    iHot = map (unindex1 . fst) shHot :: Acc (Array DIM1 Int)


indicesOf3 :: Acc (Array DIM3 Bool) -> Acc (Array DIM1 DIM3)
indicesOf3 arr = iHot
  where
    seArr = indexed arr :: Acc (Array DIM3 (DIM3, Bool))
    shHot = afst $ filter snd seArr :: Acc (Array DIM1 (DIM3, Bool))
    iHot = map fst shHot :: Acc (Array DIM1 DIM3)


showShape :: (Shape sh) => sh -> P.String
showShape = show . P.reverse . shapeToList


showShapes :: (Shape sh, Shape sh') => Array sh sh' -> P.String
showShapes arr = str
  where
    li = toList arr
    strs = P.map showShape li
    str = L.intercalate ", " strs

showGrid :: Grid -> P.String
showGrid g = show $ P.map (\b -> if b then 1 else 0) $ toList g

-- showNEligs :: Frep -> P.String
-- showNEligs f = show $ P.map (\b -> if b then 1 else 0) $ toList g
