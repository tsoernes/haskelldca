import Data.Bits (shift)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter (run)
import qualified Prelude as P
import Prelude hiding ((==))

rows = 7
cols = 7
channels = 70

type Cell = (Int, Int)

  -- neighs = fromElem (Z :. rows :. cols :. 43 :. 2 :: DIM4) 0
  -- nNeighs = fromElem (Z :. 3 :. rows :. cols :: DIM3) 0
  -- inner :: Cell ->
    -- Step 1: for a given cell (r, c),
    -- generate these non-overlapping sets as lists of neighbors:
    -- neighs1: [Cell]
    -- neighs2: [Cell]
    -- neighs4: [Cell]
    -- Step 2: add them to the 'neighs[r, c]' array, in sequence, such that all
    -- 'neighs1' come before any 'neighs2' or 'neighs4'
    -- Step 3: Set nNeigs[d, r, c] to sum([len(neighsX) for X in [0..d]])
    --
    -- Can then retrieve distance [0..d] neighbors by indexing into neighs:
    -- neighs[r, c, 0..nNeighs[d, r, c], ..]

-- (neighs, nNeighs) = fillNeighs

-- getNeighs :: Cell -> Int -> Acc (Array DIM1 Cell)
-- getNeighs (r, c) d = A.take (nNeighs ! sh1) $ slice neighs sh2
--   where
--     sh1 = constant (Z :. r :. c :. dd)
--     sh2 = constant (Z :. r :. c :. All)
--     dd = case d of
--       1 -> 0
--       2 -> 1
--       4 -> 2
    
-- fillNeighs :: (Acc (Array DIM3 Cell), Acc (Array DIM3 Int))
-- fillNeighs = (neighs2, nNeighs2)
--   where
--     sh = constant (Z :. rows :. cols :. 18) :: Exp DIM3
--     neighZeros = fill sh (lift (0 :: Int, 0 :: Int)) :: Acc (Array DIM3 Cell)
--     -- nNeighZeros = fill (constant (Z :. rows :. cols :. 3)) 0 :: Acc (Array DIM3 Int)
--     (neighs2, nNeighs2li) = foldr inner (neighZeros, []) indices
--     nNeighs2 = use $ fromList (Z :. rows :. cols :. 3) nNeighs2li
--     -- Generate indices by varying column fastest. This assures that fromList, which fills
--     -- the array in row-major order, gets nNeighs in the correct order.
--     indices = foldr (\r acc -> foldr (\c acc2 -> (r, c):acc2 ) acc [0..cols-1]) [] [0..rows-1]
--     inner :: Cell
--       -> (Acc (Array DIM3 Cell), [Int])
--       -> (Acc (Array DIM3 Cell), [Int])
--     inner cell (neighs, nNeighs) = (newNeighs, l1 : l2 : l4 : nNeighs)
--       where
--         (newNeighs, (l1, l2, l4)) = fillCellX cell neighs
        
-- fillCellX :: Cell -> Acc (Array DIM3 Cell) -> (Acc (Array DIM3 Cell), (Int, Int, Int))
-- fillCellX (r, c) arr = (permute const arr indcomb neighs2arr, (l1, l2, l4))
--   where
--     (ra, ca) = (lift r, lift c) :: (Exp Int, Exp Int)
--     (neighs2li, l1, l2, l4) = generateNeighs (r, c)
--     neighs2arr = use $ fromList (Z :. l1) neighs2li
--     -- Traverse the 3rd dimension of the given cell
--     indcomb :: Exp DIM1 -> Exp DIM3
--     indcomb nsh = index3 ra ca (unindex1 nsh)

-- (neighs2, nNeighs2) = fillNeighs2

-- getNeighs2 :: Cell -> Acc (Array DIM1 Cell)
-- getNeighs2 (r, c) = A.take (nNeighs2 ! sh1) $ slice neighs2 sh2
--   where
--     sh1 = constant (Z :. r :. c)
--     sh2 = constant (Z :. r :. c :. All)

-- fillNeighs2 :: (Acc (Array DIM3 Cell), Acc (Array DIM2 Int))
-- fillNeighs2 = (neighs2, nNeighs2)
--   where
--     sh = constant (Z :. rows :. cols :. 18) :: Exp DIM3
--     neighZeros = fill sh (lift (0 :: Int, 0 :: Int)) :: Acc (Array DIM3 Cell)
--     -- nNeighZeros = fill (constant (Z :. rows :. cols)) 0 :: Acc (Array DIM2 Int)
--     (neighs2, nNeighs2li) = foldr inner (neighZeros, []) indices
--     nNeighs2 = use $ fromList (Z :. rows :. cols) nNeighs2li
--     -- Generate indices by varying column fastest. This assures that fromList, which fills
--     -- the array in row-major order, gets nNeighs in the correct order.
--     indices = foldr (\r acc -> foldr (\c acc2 -> (r, c):acc2 ) acc [0..cols-1]) [] [0..rows-1]
--     inner :: Cell
--       -> (Acc (Array DIM3 Cell), [Int])
--       -> (Acc (Array DIM3 Cell), [Int])
--     inner cell (neighs, nNeighs) = (newNeighs, n : nNeighs)
--       where
--         (newNeighs, n) = fillCell cell neighs

-- -- Given an cell and a 3D array to contain cell neighbors,
-- -- fill in the neighbors for the given cell
-- -- and return the number of neighbors filled in
-- fillCell :: Cell -> Acc (Array DIM3 Cell) -> (Acc (Array DIM3 Cell), Int)
-- fillCell (r, c) arr = (permute const arr indcomb neighs2arr, nNeighs)
--   where
--     (ra, ca) = (lift r, lift c) :: (Exp Int, Exp Int)
--     neighs2li = generateNeighsO 2 (r, c)
--     nNeighs = P.length neighs2li
--     neighs2arr = use $ fromList (Z :. nNeighs) neighs2li
--     -- Traverse the 3rd dimension of the given cell
--     indcomb :: Exp DIM1 -> Exp DIM3
--     indcomb nsh = index3 ra ca (unindex1 nsh)


generateNeighsO :: Int -> Cell -> [Cell]
generateNeighsO d cell1 = [ (row2, col2)
                            | row2 <- [0..rows]
                            , col2 <- [0..cols]
                            , hexDistance cell1 (row2, col2) P.== d]


-- For a given cell, return a list of cell neighbors with distance 1, 2 and 4 in order
-- and the number of distance 1 neighbors, dist1+dist2 neighbors, dist1+dist2+dist4 neighbors.
generateNeighs :: Cell -> ([Cell], Int, Int, Int)
generateNeighs cell1 = (neighs1 P.++ neighs2 P.++ neighs4, l1, l2, l4)
  where
    (neighs1, neighs2, neighs4) = foldr inner ([], [], []) idxs
    idxs = [(row2, col2) | row2 <- [0..rows], col2 <- [0..cols]]
    l1 = P.length neighs1
    l2 = l1 + P.length neighs2
    l4 = l2 + P.length neighs4
    inner :: Cell -> ([Cell], [Cell], [Cell]) -> ([Cell], [Cell], [Cell])
    inner cell2 (n1, n2, n4) = case hexDistance cell1 cell2 of
      1 -> (cell2 : n1, n2, n4)
      2 -> (n1, cell2 : n2, n4)
      4 -> (n1, n2, cell2 : n4)
      _ -> (n1, n2, n4)

-- Manhattan distance between two cells in an hexagonal grid with an axial coordinate system
hexDistance :: Cell -> Cell -> Int
hexDistance (r1, c1) (r2, c2) = shift (abs rd + abs (rd + cd) + abs cd) (-1)
  where
    rd = r1 - r2
    cd = c1 - c2

neighborhood n (i,j) = (i,j) : concat [ periphery k (i,j) | k <- [1..n] ]

periphery d (i,j) = 
  let 
    ((i1,j1):ps1) = reverse . take (d+1) . iterate (\(i,j)->(i,j+1)) $ (i-d,j)
    ((i2,j2):ps2) = reverse . take (d+1) . iterate (\(i,j)->(i+1,j)) $ (i1,j1)
    ((i3,j3):ps3) = reverse . take (d+1) . iterate (\(i,j)->(i+1,j)) $ (i2,j2)
    ((i4,j4):ps4) = reverse . take (d+1) . iterate (\(i,j)->(i+1,j)) $ (i3,j3)
    ((i5,j5):ps5) = reverse . take (d+1) . iterate (\(i,j)->(i+1,j)) $ (i4,j4)
    ps6           = reverse . take (d+1) . iterate (\(i,j)->(i+1,j)) $ (i5,j5)
  in filter isValid (ps6 ++ ps5 ++ ps4 ++ ps3 ++ ps2 ++ ps1)

isValid :: Cell -> Bool
isValid (r, c)
  | r < 0 = False
  | c < 0 = False
  | r >= rows = False
  | c >= cols = False
  | otherwise = True

