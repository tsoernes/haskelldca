-- {-# LANGUAGE FlexibleContexts #-}

module Scratch where


-- import Data.Array.Accelerate (Elt, Scalar, Exp, Acc, Vector, Matrix, Array, Slice, Shape, DIM0, DIM1, DIM2, DIM3, DIM4, Z(..), (:.)(..), constant, index1, index2, index3, unindex1, unindex2, unindex3, (:.)(..), All(..), Z(..), arrayShape, arraySize, Exp, slice, boolToInt, the, unlift)
-- import qualified Data.Array.Accelerate as A
-- import Prelude as P


-- type Grid = Acc (Array DIM3 Bool)
-- type Cell = (Int, Int)

-- main' :: IO ()
-- main' = do
--   let gridI = A.map boolToInt mkGrid :: Acc (Array DIM3 Int)
--       x = run $ A.imap fn gridI
--       -- A non-sensical computation used to demonstrated that you can't call
--       -- 'getNeighboorhoodAcc' without 
--       fn :: Exp DIM3 -> Exp Int -> Exp Int
--       fn sh' _ = A.fst $ neighs4 A.! constant (Z :. 0)
--         where
--           (r', c', ch) = unlift $ unindex3 sh' :: (Exp Int, Exp Int, Exp Int)
--           neighs4 = getNeighboorhoodAcc 4 (r', c') False :: Acc (Array DIM1 Cell)
--   return ()


-- neighs :: Acc (Array DIM1 Cell)
-- nNeighs :: Acc (Array DIM3 Int)
-- (neighs, nNeighs) = undefined

-- -- Return a vector of indices of cells at distance 'd' or less from the given cell.
-- -- The boolean parameter determines if the given cell itself is included in the list.
-- getNeighboorhoodAcc :: Int -> (Exp Int, Exp Int) -> Bool -> Acc (Array DIM1 Cell)
-- getNeighboorhoodAcc d (r,c) includeself = A.take n $ A.drop start' neighs
--   where
--     start = nNeighs A.! index3 r c 0
--     start' = if includeself then start else start + 1
--     n = nNeighs A.! index3 r c (A.lift d)