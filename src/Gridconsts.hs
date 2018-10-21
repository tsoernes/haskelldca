{-# LANGUAGE TemplateHaskell #-}

module Gridconsts where

import           Base
import           AccUtils
import           Data.Array.Accelerate
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU (runQ, runN)
import Gridneighs


inp = scalar ()
generateNeighsAcc'' = CPU.runN generateNeighsAcc'
-- generateNeighsAcc'' =  $( CPU.runQ generateNeighsAcc' )
generateNeighs2Acc'' = CPU.runN generateNeighs2Acc'
neighSegments' = CPU.runN neighSegments

generateSegs' = CPU.runN generateSegs

-- (V  , V' ,  M)
_neighs :: Acc (Array DIM1 Cell)
_neighsO :: Acc (Array DIM1 Cell) -- A neighborhood does not include the focal cell (neighbors ONLY)
_nNeighs :: Acc (Array DIM3 Int)
(_neighs, _neighsO, _nNeighs) = unlift $ use a
  where
    a = generateNeighsAcc'' inp


  -- NOTE These below here seem to get compiled every iteration,
-- if verifyReuseConstraint is on (the only place they get used after iter 1)
_segs2 :: Acc (Array DIM1 Int)
_segs2 = use $ generateSegs' $ scalar 2

_segs2O :: Acc (Array DIM1 Int)
_segs2O = use $ neighSegments' $ scalar 2

_segs4O :: Acc (Array DIM1 Int)
_segs4O = use $ neighSegments' $ scalar 4

-- Like _neighs (i.e. V), but the neighborhood goes to distance 2 instead of 4.
_neighs2 :: Acc (Array DIM1 Cell)
_neighs2 = use $ generateNeighs2Acc'' inp




-- Return a vector of indices of cells at distance 'd' or less from the given cell.
-- The boolean parameter determines if the given cell itself is included in the list.
getNeighborhoodAcc :: Int -> (Exp Int, Exp Int) -> Bool -> Acc (Array DIM1 Cell)
getNeighborhoodAcc d cell includeself =
  getNeighborhoodAcc' (constant d) (lift cell) (constant includeself)


getNeighborhoodAcc' :: Exp Int -> Exp Cell -> Exp Bool -> Acc (Array DIM1 Cell)
getNeighborhoodAcc' d cell includeself = slit start n _neighs
  where
    T2 start n = getNeighborhoodOffsets d cell includeself


getNeighborhoodOffsets :: Exp Int -> Exp Cell -> Exp Bool -> Exp Cell
getNeighborhoodOffsets d (T2 r c) includeself = A.lift (start', n')
  where
    start = _nNeighs A.! index3 r c 0
    n = _nNeighs A.! index3 r c d
    -- If not includeself then start the slice 1 pos to the right,
    -- but end it as same pos as if includeself.
    start' = start + boolToInt (A.not includeself)
    n' = n - boolToInt (A.not includeself)


-- | For a given 'd', a vector [n1, n2, n3, n4, ..., nK],
-- | (where 'K' equals 'rOWS*cHANNELS'),
-- | where 'nk' is the size of the d-distance neighborhood (not including self)
-- | for cell with linear index 'k' (row-major order).
neighSegments :: Acc (Scalar Int) -> Acc (Array DIM1 Int)
neighSegments (A0 d) = A.map (\s -> s - 1) neigh_segs -- Don't count self
  where
    neigh_segs = flatten $ slice _nNeighs (lift (Z :. All :. All :. (d::Exp Int)))


-- | For a given 'd', a vector [1, n1, 1, n2, 1, n3, 1, n4, ..., 1, nK],
-- | (where 'K' equals 'rOWS*cHANNELS'),
-- | where 'nk' is the size of the d-distance neighborhood (not including self)
-- | for cell with linear index 'k' (row-major order).
generateSegs :: Acc (Scalar Int) -> Acc (Array DIM1 Int)
generateSegs d = segs
  where
    neigh_segs = neighSegments d
    -- Intersperse segments of length 1 at every even index starting at 0
    focal_segs = fill (lift (Z :. 2 * A.length neigh_segs)) (constant 1)
    segs = scatter (enumFromStepN (index1 $ A.length neigh_segs) 1 2) focal_segs neigh_segs
