{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Gridfuncs where

import Base
import Control.Arrow ((&&&), (***), first)
import Data.Array.Accelerate
import AccUtils
import Gridneighs
import qualified Prelude as P
import Debug.Trace (trace)

mkGrid :: Grid
mkGrid = fill (constant (Z :. rOWS :. cOLS :. cHANNELS)) (lift False)

mkFrep :: Frep
mkFrep = fill (constant (Z :. rOWS :. cOLS :. cHANNELS + 1)) 0

mkFreps :: Exp Int -> Freps
mkFreps i = fill (index4 (lift i) (lift rOWS) (lift cOLS) (lift cHANNELS + 1)) 0

-- | One-hot map of channels in use at cell neighbors with distance of 2 or less
inuseNeighsMap :: Cell -> Grid -> GridCell
inuseNeighsMap cell grid =
  P.foldl1 (zipWith (||)) (P.map (`sliceCell` grid) neighs)
  where
    neighs = getNeighs 2 cell False

-- | One-hot array of eligible channels
eligibleMap :: Cell -> Grid -> GridCell
eligibleMap cell grid = map not notEmap
  where
    neighs = getNeighs 2 cell False
    -- One-hot vector where 'true' means that a channel is not eligible
    notEmap = P.foldl bitor (sliceCell cell grid) neighs
    bitor :: GridCell -> Cell -> GridCell
    bitor gc cell' = zipWith (||) gc (sliceCell cell' grid)

-- | Get the allocation map of a particular cell
sliceCell :: Cell -> Grid -> GridCell
sliceCell (r, c) grid = slice grid (constant (Z :. r :. c :. All))

-- | Return the eligible channels for the given cell. A channel is eligible if it is free at
-- | the cell and all of its neighbors with distance of 2 or less.
eligibleChs :: Cell -> Grid -> Chs
eligibleChs cell grid = indicesOf $ eligibleMap cell grid

-- | Return the channels in use at the given cell.
inuseChs :: Cell -> Grid -> Chs
inuseChs cell grid = indicesOf $ sliceCell cell grid

-- | Convert a one-hot vector (sparse repr) to a vector of indecies (dense repr)
indicesOf :: Acc (Array DIM1 Bool) -> Acc (Array DIM1 Int)
indicesOf arr = iHot
    -- Pair up every element with its index (as Shape)
  where
    seArr = indexed arr :: Acc (Array DIM1 (DIM1, Bool))
    -- Filter out (index, elem) pairs where elem is not True
    seHot = filter snd seArr :: Acc (Array DIM1 (DIM1, Bool), Array DIM0 Int)
    -- Keep only the indices (as ints)
    pair = unlift seHot :: (Acc (Array DIM1 (DIM1, Bool)), Acc (Array DIM0 Int))
    sHot = P.fst pair :: Acc (Array DIM1 (DIM1, Bool))
    iHot = map (unindex1 . fst) sHot :: Acc (Array DIM1 Int)

-- Throws an error if the reuse constraint is violated
-- validateReuseConstraint :: Grid -> Bool
-- validateReuseConstraint grid = P.any P.not $ P.map checkIdx gridIdxs
--   where
--     checkIdx :: Cell -> Bool
--     checkIdx idx = undefined
--       where
--         inuseNeighs = (inuseMap idx grid) :: GridCell
--         inuseSelf = (sliceCell idx grid) :: GridCell
--         -- viols = any P.id $ zipWith (&&) inuseSelf inuseNeighs :: Acc (Scalar Bool)
--         viols = any P.id $ zipWith (&&) inuseSelf inuseNeighs :: Acc (Array DIM0 Bool)
--         xx = run viols :: Scalar Bool
--         yy = unlift xx
--         tviols = the viols :: Exp Bool

afterstate :: Grid -> Cell -> EType -> Exp Int -> Grid
afterstate grid (r, c) etype ch = permute const grid idxmap vals
  where
    vals = unit (lift (P.not $ isEnd etype)) :: Acc (Array DIM0 Bool)
    idxmap :: Exp DIM0 -> Exp DIM3
    idxmap _ = index3 (lift r) (lift c) ch

-- | Given a grid 'grid' and a set of actions, the latter specified by a cell, an event type
-- | and a list of channels, return the grids ('afterstates') that would result from
-- | executing each of the actions on 'grid'
afterstates :: Grid -> Cell -> EType -> Chs -> Grids
afterstates grid (r, c) etype chs = permute const grids idxmap vals
  where
    vals =
      fill (index1 $ length chs) (lift (P.not $ isEnd etype)) :: Acc (Array DIM1 Bool)
    grids = replicate (lift (Z :. length chs :. All :. All :. All)) grid
    idxmap :: Exp DIM1 -> Exp DIM4
    idxmap nsh = index4 (unindex1 nsh) (lift r) (lift c) (lift $ chs ! nsh)

afterStateFreps :: Grid -> Cell -> EType -> Chs -> Freps
afterStateFreps grid cell@(r, c) etype chs = nInuse ++ nElig
  where
    afsB = afterstates grid cell etype chs
    afsI = map boolToInt afsB :: Acc (Array DIM4 Int)
    nInuse = imap inuse afsI
    nElig = imap elig afsI
    -- For each row and cell, the number of eligible channels.
    elig :: Exp DIM4 -> Exp Int -> Exp Int
    elig sh' _ = the $ sum $ map boolToInt $ eligibleMap (r, c) as
      where
        (i, r', c', ch) = unindex4 sh'
        as_sh = lift (Z :. (i :: Exp Int) :. All :. All :. All)
        as = slice afsB as_sh :: Acc (Array DIM3 Bool)
    -- For a given cell, the number of times the channel is used by
    -- neighbors with distance of 4 or less.
    inuse :: Exp DIM4 -> Exp Int -> Exp Int
    inuse sh' _ = fst $ iterate n fn2 (lift (0::Int, start))
      where
        (i, r', c', ch) = unindex4 sh'
        (start, n) = getNeighborhoodOffsets 4 (r', c') False
        fn2 :: Exp (Int, Int) -> Exp (Int, Int)
        fn2 acc = lift (acc_sum + afsI ! index4 i rn cn ch, acc_idx + 1)
          where
            (acc_sum, acc_idx) = unlift acc :: (Exp Int, Exp Int)
            (rn, cn) = unlift $ neighs ! index1 acc_idx


-- | Given a grid, its feature representation frep,
-- | and a set of actions specified by cell, event type and a list of channels,
-- | derive feature representations for the afterstates of grid.
-- TO THOSE THAT VENTURE HERE:
-- There is 3 not-so-easy steps to grokking the code below:
-- 1: Understand how a feature representation is built from scratch (see `featureRep`)
-- 2: Understand how to modify the frep of the current grid to reflect the outcome of a possible action
-- (a cell, an etype, and a set of chs together specify a set of actions)
-- (see e.g. the `incremental_freps` function in `gridfuncs_numba.py` in the Python DCA project)
-- 3: Then do #2, for all actions that are possible in the current state,
-- without introducing nested data parallelism.
incAfterStateFreps :: Grid -> Frep -> Cell -> EType -> Chs -> Freps
incAfterStateFreps grid frep cell@(r, c) etype chs = nInuse ++ nElig
  where
    -- The value of a feature will change by (+1) or (-1) depending on the event and
    -- feature type, if it changes at all.
    diff = if isEnd etype then -1 else 1
    grid' = if isEnd etype then permute const grid fillCell (fill (shape chs) (lift False)) else grid
    fillCell sh = lift (Z :. r :. c :. unindex1 sh)

    afreps = replicate (lift (Z :. length chs :. All :. All :. All)) frep :: Freps
    neighs2 = getNeighborhoodAcc 2 ((lift *** lift) cell) True
    neighs4 = getNeighborhoodAcc 4 ((lift *** lift) cell) False

    nIinit = init afreps
    nInuse = permute (+) nIinit icomb $ fill (index2 (length chs) (length neighs4)) diff
    icomb :: Exp DIM2 -> Exp DIM4
    icomb sh = lift (Z :. i :. r' :. c' :. ch)
      where
        (i, j) = unlift $ unindex2 sh :: (Exp Int, Exp Int)
        ch = chs ! index1 i
        (r', c') = unlift $ neighs4 ! index1 j :: (Exp Int, Exp Int)

    nEinit = drop (lift cHANNELS) afreps
    nElig = permute (+) nEinit idxComb eligDiff'
    -- There is 1 entry in 'eligDiff' for each frep in 'afreps';
    -- or equivalently, for each ch in 'chs'.
    idxComb :: Exp DIM2 -> Exp DIM4
    idxComb sh = lift (Z :. i :. r' :. c' :. (0 :: Int))
      where
        (i, j) = unlift $ unindex2 sh :: (Exp Int, Exp Int)
        (r', c') = unlift $ neighs2 ! index1 j  :: (Exp Int, Exp Int)

    eligDiff = replicate (lift (Z :. All :. length neighs2)) chs
    eligDiff' = imap mapCh eligDiff
    mapCh :: Exp DIM2 -> Exp Int -> Exp Int
    mapCh sh ch = (-1) * diff * (boolToInt . not $ notElig)
      where
        (i, j) = unlift $ unindex2 sh :: (Exp Int, Exp Int) -- (ch_idx, neigh2_idx)
        (r', c') = unlift $ neighs2 ! index1 j  :: (Exp Int, Exp Int)
        (start, n) = getNeighborhoodOffsets 2 (r', c') True :: (Exp Int, Exp Int)
        -- `notElig` is True if the channel is not eligible.
        notElig = fst $ iterate n fn (lift (False, start))
        fn :: Exp (Bool, Int) -> Exp (Bool, Int)
        fn acc = lift (acc_diff || neighBit, acc_idx + 1)
          where
            (acc_diff, acc_idx) = unlift acc :: (Exp Bool, Exp Int)
            (r'', c'') = unlift $ neighs ! index1 acc_idx
            neighBit = grid' ! index3 r'' c'' ch

-- | A feature representation (frep for short) of the grid. The frep is of the
-- | same spatial dimension as the grid but 1 longer in depth.
-- | For a given cell, the first 'cHANNELS' features
-- | specifies how many times each of the channels is in used within a 4-cell radius,
-- | not including the cell itself. An additional feature counts the number of eligible
-- | channels in that cell.
featureRep :: Grid -> Frep
featureRep grid = P.foldl fn zeros gridIdxs
  where
    zeros = fill (constant (Z :. rOWS :. cOLS :. cHANNELS + 1)) 0
    -- Fill in 1 cell at a time
    fn :: Frep -> Cell -> Frep
    fn frep (r, c) = permute const frep idxmap (nUsed ++ nElig)
      where
        nUsedZ = fill (constant (Z :. cHANNELS + 1)) 0 :: Acc (Vector Int)
        neighs4 = getNeighs 4 (r, c) False
        nUsed =
          P.foldl
            (\acc cell -> zipWith (+) acc (map boolToInt $ sliceCell cell grid))
            nUsedZ
            neighs4
        nElig = reshape (constant (Z :. 1)) . boolSum $ eligibleMap (r, c) grid
        -- Traverse the 3rd dimension of the given cell
        idxmap :: Exp DIM1 -> Exp DIM3
        idxmap nsh = index3 (lift r) (lift c) (unindex1 nsh)

incrementalFreps :: Grid -> Frep -> Cell -> EType -> Chs -> Freps
incrementalFreps = undefined

-- | Switch bit at given cell off for END events; on for NEW events
executeAction :: Event -> Ch -> Grid -> Grid
executeAction Event {_evType, _evCell = (r, c)} toCh grid =
  let val = unit $ lift $ P.not $ isEnd _evType
      setIdx _ = constant (Z :. r :. c :. toCh)
   in permute const grid setIdx val