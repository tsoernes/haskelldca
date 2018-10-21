module Gridneighs where

import           Base ( cOLS, gridIdxs, rOWS, Cell, Backend(CPU) )

import           AccUtils
import           Data.Array.Accelerate
    ( boolToInt, the, fromList, unit, Scalar, use, Exp, Acc, Array, DIM1, DIM2, DIM3, index3, (:.)((:.)), Z(Z), unlift, cond, slit, flatten, scatter, slice, constant, fill, lift, enumFromStepN, index1, All(..) )
import qualified Data.Array.Accelerate as A

-- -- (V  , V' ,  M)
-- _neighs :: Acc (Array DIM1 Cell)
-- _neighsO :: Acc (Array DIM1 Cell) -- A neighborhood does not include the focal cell (neighbors ONLY)
-- _nNeighs :: Acc (Array DIM3 Int)
-- (_neighs, _neighsO, _nNeighs) = unlift $ use $ result
--   where
--     inp = scalar ()
--     f = $( CPU.runQ generateNeighsAcc' )
--     a = f inp


--   -- NOTE These below here seem to get compiled every iteration,
-- -- if verifyReuseConstraint is on (the only place they get used after iter 1)
-- _segs2 :: Acc (Array DIM1 Int)
-- _segs2 = generateSegs 2
-- -- _segs2 = recycle1E generateSegs 2

-- _segs2O :: Acc (Array DIM1 Int)
-- _segs2O = neighSegments 2
-- -- _segs2O = recycle1E neighSegments 2

-- _segs4O :: Acc (Array DIM1 Int)
-- _segs4O = neighSegments 4
-- -- _segs4O = recycle1E neighSegments 4

-- -- Like _neighs (i.e. V), but the neighborhood goes to distance 2 instead of 4.
-- _neighs2 :: Acc (Array DIM1 Cell)
-- _neighs2 = generateNeighs2Acc
-- -- _neighs2 = use $ run1S CPU generateNeighs2Acc



-- Returns a vector V of cells and (V' and) a 3D-array M of integers.
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
generateNeighsAcc' :: Acc (Scalar ()) -> Acc (Array DIM1 Cell, Array DIM1 Cell, Array DIM3 Int)
generateNeighsAcc' _ = lift generateNeighsAcc
generateNeighsAcc :: (Acc (Array DIM1 Cell), Acc (Array DIM1 Cell), Acc (Array DIM3 Int))
generateNeighsAcc = (neighsArr, neighsArrO, nNeighsArr)
  where
    -- First generate the results  as two flat Lists,
    -- then populate a vector and a matrix.
    nNeighsArr = A.use $ A.fromList (Z :. rOWS :. cOLS :. 5) nNeighsLi
    neighsArr = A.use $ A.fromList (Z :. n) neighsLi
    neighsArrO = A.use $ A.fromList (Z :. nO) neighsLiO
    -- 'n' counts how many neighbor indices (by pairs) that's stored in
    -- 'nNeighsLi' thus far
    (neighsLi, neighsLiO, nNeighsLi, n, nO) = foldl inner ([], [], [], 0, 0) gridIdxs
    inner (neighs', neighsO', nNeighs', n', nO') idx = (neighs'', neighsO'', nNeighs'', n'', nO'')
      where
        (cellNeighs@(_ : cellNeighs'), cellNNeighs) = neighborhood 4 idx
        neighsO'' = neighsO' ++ cellNeighs'
        neighs'' = neighs' ++ cellNeighs
        n'' = n' + length cellNeighs
        nO'' = nO' + length cellNeighs - 1
        nNeighs'' = nNeighs' ++ n' : cellNNeighs


generateNeighs2Acc' :: Acc (Scalar ()) -> Acc (Array DIM1 Cell)
generateNeighs2Acc' _ = generateNeighs2Acc
generateNeighs2Acc :: Acc (Array DIM1 Cell)
generateNeighs2Acc = neighsArr
  where
    neighsArr = A.use $ A.fromList (Z :. length neighsLi) neighsLi
    neighsLi = foldl inner [] gridIdxs
    inner neighs' idx = neighs' ++ cellNeighs
      where
        (cellNeighs, _) = neighborhood 2 idx


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
