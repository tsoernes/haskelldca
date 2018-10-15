{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Gridfuncs where

import           AccUtils
import           Base
import           Data.Array.Accelerate
import           Gridneighs
import qualified Prelude as P
import Opt
import Control.Monad.Reader (MonadReader, asks)


-- | Only used internally for testing
afterstate :: Cell -> Ch -> Bool -> Acc Grid -> Acc Grid
afterstate (r, c) ch v grid = permute const grid setIdx (unit $ lift v)
  where
    setIdx _ = constant (Z :. r :. c :. ch) :: Exp DIM3


mkGrid :: (MonadReader Opt m) => m Grid
mkGrid = do
  bend <- asks backend
  P.return $ run bend $ fill (constant (Z :. rOWS :. cOLS :. cHANNELS)) (lift False)


mkAGrid :: Acc Grid
mkAGrid = fill (constant (Z :. rOWS :. cOLS :. cHANNELS)) (lift False)


mkFrep :: (MonadReader Opt m) => m Frep
mkFrep = do
  bend <- asks backend
  P.return $ run bend $ fill (constant (Z :. rOWS :. cOLS :. cHANNELS + 1)) 0


-- | Get the allocation map of a particular cell
sliceCell :: Exp Cell -> Acc Grid -> Acc GridCell
sliceCell (T2 r c) grid = slice grid (lift (Z :. r :. c :. All))


-- | Given a list of cells, slice them out and put the GridCells in a 1D vector.
sliceNeighs :: Acc (Array DIM1 Cell) -> Acc Grid -> Acc (Array DIM2 Bool)
sliceNeighs neighs grid = allNeighs
  where
    allNeighsSh = lift (Z :. length neighs :. cHANNELS) :: Exp DIM2
    allNeighs = backpermute allNeighsSh ixTrans grid
    ixTrans :: Exp DIM2 -> Exp DIM3
    ixTrans (D2 i ch) = lift (Z :. r :. c :. ch)
      where
        T2 r c = neighs ! index1 i


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


-- | One-hot map of channels in use at cell neighbors with distance of 2 or less
inuseNeighsMap :: Exp Cell -> Acc Grid -> Acc GridCell
inuseNeighsMap cell grid = fold1 (||) (transpose allNeighs)
  where
    neighs = getNeighborhoodAcc' 2 cell (constant False)
    allNeighs = sliceNeighs neighs grid


-- | One-hot array of eligible channels
eligibleMap :: Exp Cell -> Acc Grid -> Acc GridCell
eligibleMap cell grid = map not notEmap
  where
    -- Gather alloc map of each neigh into an array
    neighs = getNeighborhoodAcc' 2 cell (constant True)
    allNeighs = sliceNeighs neighs grid
    -- One-hot vector where 'true' means that a channel is not eligible
    notEmap = fold1 (||) (transpose allNeighs)


-- | Return the eligible channels for the given cell. A channel is eligible if it is free
-- | (i.e. False; not in use) in the cell and all of its neighbors with distance of 2 or less.
eligibleChs :: Exp Cell -> Acc Grid -> Acc Chs
eligibleChs cell grid = indicesOf $ eligibleMap cell grid


-- | One-hot array of eligible channels
-- | Return the channels in use at the given cell.
-- | Return the channels in use at the given cell.
inuseChs :: Exp Cell -> Acc Grid -> Acc Chs
inuseChs cell grid = indicesOf $ sliceCell cell grid


-- | Return 'True' if the reuse constraint is violated.
-- | (WHICH SHOULD NEVER HAPPEN; there's a bug in the program)
-- | If a channel is in use at a focal cell and any of
-- | neighbors withing distance of 2, then the reuse constraint is violated.
violatesReuseConstraint :: Acc Grid -> Exp Bool
violatesReuseConstraint grid = (the . or . flatten) violatesPerCh
  where
    -- C0 C0N0 C0N1 C0N2 .. C0Nx C1N0 C1N1 .. C1Ny ..
    -- 'allNeighs' should be thought of as a vector of grid cells.
    allNeighs = transpose $ sliceNeighs _neighs grid :: Acc (Array DIM2 Bool)
    alternates = fold1Seg (||) allNeighs _segs :: Acc (Array DIM2 Bool)
    twoSegs = fill (index1 . lift $ rOWS * cOLS) 2 :: Acc (Array DIM1 Int)
    violatesPerChPerCell = fold1Seg (&&) alternates twoSegs
    violatesPerCh = fold1 (||) violatesPerChPerCell


-- | Switch bit at given cell off for END events; on for NEW events
executeAction :: Exp Bool -> Exp Cell -> Exp Ch -> Acc Grid -> Acc Grid
executeAction eIsEnd (T2 r c) toCh grid =
  let val = unit $ not eIsEnd
      setIdx :: Exp DIM0 -> Exp DIM3
      setIdx _ = lift (Z :. r :. c :. toCh)
   in permute const grid setIdx val


-- | Given a grid 'grid' and a set of actions, the latter specified by a cell, an event type
-- | and a list of channels, return the grids ('afterstates') that would result from
-- | executing each of the actions on 'grid'
afterstates :: Acc Grid -> Cell -> EType -> Acc Chs -> Acc Grids
afterstates grid (r, c) etype chs = permute const grids idxmap vals
  where
    vals =
      fill (index1 $ length chs) (lift (P.not $ isEnd etype)) :: Acc (Array DIM1 Bool)
    grids = replicate (lift (Z :. length chs :. All :. All :. All)) grid
    idxmap :: Exp DIM1 -> Exp DIM4
    idxmap nsh = index4 (unindex1 nsh) (lift r) (lift c) (lift $ chs ! nsh)


-- | A feature representation (frep for short) of the grid. The frep is of the
-- | same spatial dimension as the grid but 1 longer in depth.
-- | For a given cell, the first 'cHANNELS' features
-- | specifies how many times each of the channels is in used within a 4-cell radius,
-- | not including the cell itself. An additional feature counts the number of eligible
-- | channels in that cell.
featureRep :: Acc Grid -> Acc Frep
featureRep grid = P.foldl fn zeros gridIdxs
  where
    zeros = fill (constant (Z :. rOWS :. cOLS :. cHANNELS + 1)) 0
    -- Fill in 1 cell at a time
    fn :: Acc Frep -> Cell -> Acc Frep
    fn frep (r, c) = permute const frep idxmap (nUsed ++ nElig)
      where
        nUsedZ = fill (constant (Z :. cHANNELS + 1)) 0 :: Acc (Vector Int)
        neighs4 = getNeighs 4 (r, c) False
        nUsed =
          P.foldl
            (\acc cell -> zipWith (+) acc (map boolToInt $ sliceCell (lift cell) grid))
            nUsedZ
            neighs4
        nElig = reshape (constant (Z :. 1)) . unit . boolSum $ eligibleMap (lift (r, c)) grid
        -- Traverse the 3rd dimension of the given cell
        idxmap :: Exp DIM1 -> Exp DIM3
        idxmap nsh = index3 (lift r) (lift c) (unindex1 nsh)


-- | Given a grid, its feature representation frep,
-- | and a set of actions specified by cell, event type and a list of channels,
-- | derive feature representations for the afterstates of grid incrementally.
-- | The shape of the resulting array is (Z :. n_chs :. rOWS :. cOLS :. cHANNELS + 1).
incAfterStateFreps :: Exp Cell -> Exp Bool -> Acc Chs -> Acc Grid -> Acc Frep -> Acc Freps
incAfterStateFreps cell@(T2 r c) eIsEnd chs grid frep = nInuse ++ nElig
  where
    -- TO THOSE THAT VENTURE HERE:
    -- There is 3 not-so-easy steps to grokking the code below:
    -- 1: Understand how a feature representation is built from scratch (see `featureRep`)
    -- 2: Understand how to modify the frep of the current grid to reflect the
    -- outcome of a possible action
    -- (a cell, an etype, and a set of chs together specify a set of actions)
    -- (see e.g. the `incremental_freps` function in `gridfuncs_numba.py` in the Python DCA project)
    -- 3: Then do #2, for all actions that are possible in the current state,
    -- without introducing nested data parallelism.
    ---------------------------------------------------------------------------------
    -- The value of a feature will change by (+1) or (-1) depending on the event and
    -- feature type, if it changes at all.
    diff = cond eIsEnd (-1) 1
    zgrid = permute const grid fillCell (fill (shape chs) (lift False))
    grid' = acond eIsEnd zgrid grid
    fillCell sh = lift (Z :. r :. c :. unindex1 sh)

    -- One (frep of an) afterstate for each possible action.
    afreps = replicate (lift (Z :. length chs :. All :. All :. All)) frep :: Acc Freps
    neighs2 = getNeighborhoodAcc' 2 cell (constant True)
    neighs4 = getNeighborhoodAcc' 4 cell (constant False)

    -- Compute the inuse features
    nIinit = init afreps
    nInuse = permute (+) nIinit icomb $ fill (index2 (length chs) (length neighs4)) diff
    icomb :: Exp DIM2 -> Exp DIM4
    icomb sh = lift (Z :. ch_i :. r' :. c' :. ch)
      where
        T2 ch_i neigh_i = unindex2 sh
        ch = chs ! index1 ch_i
        T2 r' c' = neighs4 ! index1 neigh_i

    -- Compute the eligibility feature
    nEinit = drop (lift cHANNELS) afreps -- Remember, we're dropping along 4th dim.
    nElig = permute (+) nEinit idxComb eligDiff'
    -- There is 1 entry in 'eligDiff' for each frep in 'afreps';
    -- or equivalently, for each ch in 'chs'.
    idxComb :: Exp DIM2 -> Exp DIM4
    idxComb sh = lift (Z :. ch_i :. r' :. c' :. (0 :: Int))
      where
        T2 ch_i neigh_i = unindex2 sh
        T2 r' c' = neighs2 ! index1 neigh_i

    -- Compute the change in eligibility feature
    eligDiff = replicate (lift (Z :. All :. length neighs2)) chs
    eligDiff' = imap mapCh eligDiff
    mapCh :: Exp DIM2 -> Exp Int -> Exp Int
    mapCh (D2 _ neighIx) ch = (-1) * diff * (boolToInt . not $ notElig)
      where
        T2 r' c' = neighs2 ! index1 neighIx
        (start, n) = getNeighborhoodOffsets 2 (r', c') True :: (Exp Int, Exp Int)
        notElig = fst $ iterate n orNeigh (lift (False, start))
        orNeigh :: Exp (Bool, Int) -> Exp (Bool, Int)
        orNeigh (T2 acc_diff acc_idx) = lift (acc_diff || neighBit, acc_idx + 1)
          where
            T2 r'' c'' = _neighs ! index1 acc_idx
            neighBit = grid' ! index3 r'' c'' ch
