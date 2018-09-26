module Gridneighs where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Acc, Array, DIM1, DIM2, DIM3, Z(..), (:.)(..), (!), fromList, use)
import Data.Array.Accelerate.Interpreter (run)
import Base

-- (neighs, nNeighs) = generateNeighs

-- -- Return a vector of indices of cells at distance 'd' or less from the given cell.
-- -- The boolean parameter determines if the given cell itself is included in the list.
-- getNeighs :: Int -> Cell -> Bool -> Acc (Array DIM1 Cell)
-- getNeighs d (r,c) includeself = A.take n $ A.drop start' neighs
--   where
--     start = nNeighs ! A.constant (Z :. r :. c :. 0)
--     start' = if includeself then start else start + 1
--     n = nNeighs ! A.constant (Z :. r :. c :. d)

-- -- The second matrix has entries for each (row, col) pair of the form
-- -- [n, n1, n2, n3, n4] where 'n' is the start position in the first vector.
-- -- The range 'neighs[n..n1]' yields the indices of neighbors with distance 1
-- -- for the cell (row, col) and similarly for n2, n3 and n4.
-- generateNeighs :: (Acc (Array DIM1 Cell), Acc (Array DIM3 Int))
-- generateNeighs = (neighsArr, nNeighsArr)
--   where
--     (neighsLi, nNeighsLi, n) = foldl inner ([], [], 0) idxs
--     neighsArr = use $ fromList (Z :. n) neighsLi
--     nNeighsArr = use $ fromList (Z :. rOWS :. cOLS :. 5) nNeighsLi
--     inner (neighs', nNeighs', n') idx = (neighs' ++ cellNeighs, nNeighs'', n'')
--       where
--         (cellNeighs, cellNNeighs) = neighborhood idx
--         n'' = n' + length cellNeighs
--         nNeighs'' = nNeighs' ++ n' : cellNNeighs


getNeighs :: Int -> Cell -> Bool -> [Cell]
getNeighs d (r,c) includeself = if includeself then (r,c) : neighs else neighs
  where
    neighs = concat [ periphery d' (r,c) | d' <- [1..d] ]

neighborhood :: Cell -> ([Cell], [Int])
neighborhood (r,c) = (neighs, nNeighs)
  where
    neighsO = [ periphery d (r,c) | d <- [1..4] ]
    neighs = (r,c) : concat neighsO
    nNeighs = tail $ scanl (+) 1 $ map length neighsO

-- Return the set of d-distance neighbors for the given cell
periphery d (r,c) =
  -- The set of d-distance neighbors form a hexagon shape. Traverse each of
  -- the sides of this hexagon and gather up the cell indices.
  let
    ps1 = take d . iterate (\(r,c)->(r,c+1))   $ (r-d,c)
    ps2 = take d . iterate (\(r,c)->(r+1,c))   $ (r-d,c+d)
    ps3 = take d . iterate (\(r,c)->(r+1,c-1)) $ (r,c+d)
    ps4 = take d . iterate (\(r,c)->(r,c-1))   $ (r+d,c)
    ps5 = take d . iterate (\(r,c)->(r-1,c))   $ (r+d,c-d)
    ps6 = take d . iterate (\(r,c)->(r-1,c+1)) $ (r,c-d)
  in filter isValid (ps6 ++ ps5 ++ ps4 ++ ps3 ++ ps2 ++ ps1)

-- Return the set of d-distance neighbors for the given cell
periphery' d (r,c) =
  -- The set of d-distance neighbors form a hexagon shape. Traverse each of
  -- the sides of this hexagon and gather up the cell indices.
  let
    ps1 = zip (repeat (r-d)) [c, c+1 .. c+d-1]
    ps2 = zip [r-d, r-d+1 .. r] (repeat (c+d))
    ps3 = zip [r, r+1 .. r+d-1] [c+d, c+d-1 .. c+1]
    ps4 = zip (repeat (r+d)) [c, c-1 .. c-d+1]
    ps5 = zip [r+d, r+d-1 .. r+1] (repeat (c-d))
    ps6 = zip [r, r-1 .. r-d] [c-d, c-d+1 .. c-1]
  in filter isValid (ps6 ++ ps5 ++ ps4 ++ ps3 ++ ps2 ++ ps1)
  
isValid :: Cell -> Bool
isValid (r, c)
  | r < 0 || r >= rOWS = False
  | c < 0 || c >= cOLS = False
  | otherwise = True
