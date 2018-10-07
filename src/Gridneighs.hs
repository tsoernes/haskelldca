module Gridneighs where

import Base ( cOLS, gridIdxs, rOWS, Cell )
import Data.Array.Accelerate
    ( Exp, Acc, Array, DIM1, DIM3, index3, (:.)((:.)), Z(Z) )
import qualified Data.Array.Accelerate as A
    ( fromList, (!), use, slit, Lift(lift) )

-- (V  , M)
_neighs :: Acc (Array DIM1 Cell)
_nNeighs :: Acc (Array DIM3 Int)
(_neighs, _nNeighs) = generateNeighsAcc

-- Return a vector of indices of cells at distance 'd' or less from the given cell.
-- The boolean parameter determines if the given cell itself is included in the list.
getNeighborhoodAcc :: Int -> (Exp Int, Exp Int) -> Bool -> Acc (Array DIM1 Cell)
getNeighborhoodAcc d cell includeself = A.slit start n _neighs
  where
    (start, n) = getNeighborhoodOffsets d cell includeself


getNeighborhoodOffsets :: Int -> (Exp Int, Exp Int) -> Bool -> (Exp Int, Exp Int)
getNeighborhoodOffsets d (r,c) includeself = (start', n')
  where
    start = _nNeighs A.! index3 r c 0
    start' = if includeself then start else start + 1
    n = _nNeighs A.! index3 r c (A.lift d)
    n' = if includeself then n else n - 1

-- Returns a vector V of cells and a matrix M of integers.
-- M is of shape RxCx5 where R and C are the number of
-- rows and columns in the grid and thus has
-- a vector (henceforth M[r, c]) of length 5 for each cell (r, c) in grid.
-- For a given cell (r, c), the vector M[r, c] contains 5 integer
-- entries that can be used to look up any
-- d-distance neighborhood for the given cell for d in the inclusive range of [1..4].
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
        (cellNeighs, cellNNeighs) = neighborhood idx
        n'' = n' + length cellNeighs
        nNeighs'' = nNeighs' ++ n' : cellNNeighs

getNeighs :: Int -> Cell -> Bool -> [Cell]
getNeighs d (r, c) includeself =
  if includeself
    then (r, c) : neighs
    else neighs
  where
    neighs = concat [periphery d' (r, c) | d' <- [1 .. d]]

neighborhood :: Cell -> ([Cell], [Int])
neighborhood (r, c) = (neighs, nNeighs)
  where
    neighsO = [periphery d (r, c) | d <- [1 .. 4]]
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

-- Return the set of d-distance neighbors for the given cell
periphery' :: Int -> Cell -> [Cell]
periphery' d (r, c)
  -- The set of d-distance neighbors form a hexagon shape. Traverse each of
  -- the sides of this hexagon and gather up the cell indices.
 =
  let ps1 = zip (repeat (r - d)) [c,c + 1 .. c + d - 1]
      ps2 = zip [r - d,r - d + 1 .. r] (repeat (c + d))
      ps3 = zip [r,r + 1 .. r + d - 1] [c + d,c + d - 1 .. c + 1]
      ps4 = zip (repeat (r + d)) [c,c - 1 .. c - d + 1]
      ps5 = zip [r + d,r + d - 1 .. r + 1] (repeat (c - d))
      ps6 = zip [r,r - 1 .. r - d] [c - d,c - d + 1 .. c - 1]
   in filter isValid (ps6 ++ ps5 ++ ps4 ++ ps3 ++ ps2 ++ ps1)

isValid :: Cell -> Bool
isValid (r, c)
  | r < 0 || r >= rOWS = False
  | c < 0 || c >= cOLS = False
  | otherwise = True
