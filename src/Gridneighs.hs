module Gridneighs where

import Base ( cOLS, gridIdxs, rOWS, Cell )
import AccUtils
import Data.Array.Accelerate
    ( Exp, Acc, Array, DIM1, DIM3, index3, (:.)((:.)), Z(Z), unlift, cond, slit, flatten, scatter, slice, constant, fill, lift, enumFromStepN, index1, All(..) )
import qualified Data.Array.Accelerate as A

-- (V  , M)
_neighs :: Acc (Array DIM1 Cell)
_nNeighs :: Acc (Array DIM3 Int)
(_neighs, _nNeighs) = generateNeighsAcc

_segs2 :: Acc (Array DIM1 Int)
_segs2 = generateSegs 2

_segs4 :: Acc (Array DIM1 Int)
_segs4 = generateSegs 4

_neighs2 = generateNeighs2Acc

-- Return a vector of indices of cells at distance 'd' or less from the given cell.
-- The boolean parameter determines if the given cell itself is included in the list.
getNeighborhoodAcc :: Int -> (Exp Int, Exp Int) -> Bool -> Acc (Array DIM1 Cell)
getNeighborhoodAcc d cell includeself = slit start n _neighs
  where
    (start, n) = getNeighborhoodOffsets d cell includeself

getNeighborhoodAcc' :: Exp Int -> Exp Cell -> Exp Bool -> Acc (Array DIM1 Cell)
getNeighborhoodAcc' d cell includeself = slit start n _neighs
  where
    (start, n) = unlift $ getNeighborhoodOffsets' d cell includeself

getNeighborhoodOffsets :: Int -> (Exp Int, Exp Int) -> Bool -> (Exp Int, Exp Int)
getNeighborhoodOffsets d (r,c) includeself = (start', n')
  where
    start = _nNeighs A.! index3 r c 0
    start' = if includeself then start else start + 1
    n = _nNeighs A.! index3 r c (A.lift d)
    n' = if includeself then n else n - 1

getNeighborhoodOffsets' :: Exp Int -> Exp Cell -> Exp Bool -> Exp Cell
getNeighborhoodOffsets' d (T2 r c) includeself = A.lift (start', n')
  where
    start = _nNeighs A.! index3 r c 0
    start' = cond includeself start (start + 1)
    n = _nNeighs A.! index3 r c (A.lift d)
    n' = cond includeself n (n - 1)

-- Returns a vector V of cells and a 3D-array M of integers.
-- M is of shape RxCx5 where R and C are the number of
-- rows and columns in the grid and thus has
-- a vector (henceforth M[r, c]) of length 5 for each cell in grid.
-- For a given cell (r, c), the vector M[r, c] contains 5 integer
-- entries that can be used to look up any d-distance neighborhood for the
-- given cell for d in the inclusive range of [1..4].
-- The definition of a d-distance neighborhood is given in the documentation of
-- the `getNeighboorhood` function which can later be used to perform
-- the retrieval of said neighborhoods.
-- The retrieval utilizes M[r, c][0] and M[r, c][d] to return a slice (range) of
-- the vector V. This slice contains the set of cells which form the d-distance
-- neighborhood of (r, c).
-- Specifically, for M[r, c]=[n, n_1, n_2, n_3, n_4], the inclusive slice of cells V[n:n_d]
-- yield the d-distance neighborhood for d in [1..4].
generateNeighsAcc :: (Acc (Array DIM1 Cell), Acc (Array DIM3 Int))
generateNeighsAcc = (neighsArr, nNeighsArr)
  where
    -- First generate the results  as two flat Lists,
    -- then populate a vector and a matrix.
    nNeighsArr = A.use $ A.fromList (Z :. rOWS :. cOLS :. 5) nNeighsLi
    neighsArr = A.use $ A.fromList (Z :. n) neighsLi
    -- 'n' counts how many neighbor indices (by pairs) that's stored in
    -- 'nNeighsLi' thus far
    (neighsLi, nNeighsLi, n) = foldl inner ([], [], 0) gridIdxs
    inner (neighs', nNeighs', n') idx = (neighs' ++ cellNeighs, nNeighs'', n'')
      where
        (cellNeighs, cellNNeighs) = neighborhood 4 idx
        n'' = n' + length cellNeighs
        nNeighs'' = nNeighs' ++ n' : cellNNeighs

generateNeighs2Acc :: Acc (Array DIM1 Cell)
generateNeighs2Acc = neighsArr
  where
    neighsArr = A.use $ A.fromList (Z :. length neighsLi) neighsLi
    neighsLi = foldl inner [] gridIdxs
    inner neighs' idx = neighs' ++ cellNeighs
      where
        (cellNeighs, _) = neighborhood 2 idx


-- | A vector [1, n1, 1, n2, 1, n3, 1, n4, ..., 1, nK],
-- | where 'nk' is the size of the d-distance neighborhood (not including self)
-- | for cell number 'k' counting in linear row-major order
-- | (that is such that 'K' equals 'rOWS*cHANNELS').
generateSegs :: Int -> Acc (Array DIM1 Int)
generateSegs d = segs
  where
    neigh_segs = flatten $ slice _nNeighs (constant (Z :. All :. All :. (d::Int)))
    neigh_segs' = A.map (\s -> s - 1) neigh_segs -- Don't count self
    -- Intersperse segments of length 1 at every even index starting at 0
    focal_segs = fill (lift (Z :. 2 * A.length neigh_segs)) (constant 1)
    segs = scatter (enumFromStepN (index1 $ A.length neigh_segs) 1 2) focal_segs neigh_segs'


getNeighs :: Int -> Cell -> Bool -> [Cell]
getNeighs d (r, c) includeself =
  if includeself
    then (r, c) : neighs
    else neighs
  where
    neighs = concat [periphery d' (r, c) | d' <- [1 .. d]]

-- For d >= 1
neighborhood :: Int -> Cell -> ([Cell], [Int])
neighborhood dHi (r, c) = (neighs, nNeighs)
  where
    neighsO = [periphery d (r, c) | d <- [1 .. dHi]]
    neighs = (r, c) : concat neighsO
    nNeighs = tail $ scanl (+) 1 $ map length neighsO

-- Return the set of d-distance neighbors for the given cell
periphery :: Int -> Cell -> [Cell]
periphery d (rFoc, cFoc) =
  -- The set of d-distance neighbors form a hexagon shape. Traverse each of
  -- the sides of this hexagon and gather up the cell indices.
  let ps1 = take d . iterate (\(r, c) -> (r, c + 1)) $ (rFoc - d, cFoc)
      ps2 = take d . iterate (\(r, c) -> (r + 1, c)) $ (rFoc - d, cFoc + d)
      ps3 = take d . iterate (\(r, c) -> (r + 1, c - 1)) $ (rFoc, cFoc + d)
      ps4 = take d . iterate (\(r, c) -> (r, c - 1)) $ (rFoc + d, cFoc)
      ps5 = take d . iterate (\(r, c) -> (r - 1, c)) $ (rFoc + d, cFoc - d)
      ps6 = take d . iterate (\(r, c) -> (r - 1, c + 1)) $ (rFoc, cFoc - d)
   in filter isValid (ps6 ++ ps5 ++ ps4 ++ ps3 ++ ps2 ++ ps1)


isValid :: Cell -> Bool
isValid (r, c)
  | r < 0 || r >= rOWS = False
  | c < 0 || c >= cOLS = False
  | otherwise = True
