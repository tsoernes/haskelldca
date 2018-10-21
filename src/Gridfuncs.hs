{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Gridfuncs where

import           AccUtils
import           Base
import           Data.Array.Accelerate
import           Gridneighs
import           Gridconsts
import qualified Prelude as P
import Opt
import Control.Monad.Reader (MonadReader, asks)


-- | Only used internally for testing
afterstate :: Cell -> Ch -> Bool -> Acc Grid -> Acc Grid
afterstate cell = afterstate' (constant cell)


afterstate' :: Exp Cell -> Ch -> Bool -> Acc Grid -> Acc Grid
afterstate' (T2 r c) ch val grid = permute const grid setIdx (unit $ lift val)
  where
    setIdx _ = lift (Z :. r :. c :. ch) :: Exp DIM3


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


-- | Given a vector of cells, slice them out from the grid, and put the GridCells in a 1D vector.
sliceNeighs :: (Elt e) => Acc (Array DIM1 Cell) -> Acc (Array DIM3 e) -> Acc (Array DIM2 e)
sliceNeighs neighs grid = allNeighs
  where
    allNeighsSh = lift (Z :. length neighs :. cHANNELS) :: Exp DIM2
    allNeighs = backpermute allNeighsSh ixTrans grid
    ixTrans :: Exp DIM2 -> Exp DIM3
    ixTrans (D2 i ch) = lift (Z :. r :. c :. ch)
      where
        T2 r c = neighs ! index1 i


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


-- | Return the channels in use at the given cell.
inuseChs :: Exp Cell -> Acc Grid -> Acc Chs
inuseChs cell grid = indicesOf $ sliceCell cell grid


-- | Return 'True' if the reuse constraint is violated.
-- | (WHICH SHOULD NEVER HAPPEN; there's a bug in the program)
-- | If a channel is in use at a focal cell simultaneously as any of its
-- | neighbors within a distance of 2, then the reuse constraint is violated.
violatesReuseConstraint :: Acc Grid -> Exp Bool
violatesReuseConstraint grid = (the . or . flatten) violatesPerCh
  where
    allGCNeighs = transpose $ sliceNeighs _neighs2 grid :: Acc (Array DIM2 Bool)
    alternates = fold1Seg (||) allGCNeighs _segs2 :: Acc (Array DIM2 Bool)
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
featureRep' :: Acc Grid -> Acc Frep
featureRep' grid = P.foldl fn zeros gridIdxs
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


featureRep :: Acc Grid -> Acc Frep
featureRep grid = nInuse ++ nElig
  where
    grid' = map boolToInt grid
    allGCNeighs4 = transpose $ sliceNeighs _neighsO grid'
    -- A segmented fold reduces along the innermost dimension, thus
    -- for a particular channel `ch` and segment `s` the usages `nInuseFlat[ch][s]`
    -- of all the neighbors in the 4-distance neighborhood
    -- of some particular cell determined by `s`.
    nInuseFlat = fold1Seg (+) allGCNeighs4 _segs4O :: Acc (Array DIM2 Int)
    nInuse = reshape (constant (Z :. rOWS :. cOLS :. cHANNELS)) $ transpose nInuseFlat

    allGCNeighs2 = transpose $ sliceNeighs _neighs2 grid
    nbHoods = map (+1) _segs2O :: Acc (Array DIM1 Int)
    isElig = map not $ fold1Seg (||) allGCNeighs2 nbHoods :: Acc (Array DIM2 Bool)
    -- Count the number of eligible channels for each cell
    nEligFlat = sum $ map boolToInt $ transpose isElig
    nElig = reshape (constant (Z :. rOWS :. cOLS :. 1)) nEligFlat


-- | Get a single frep incrementally. Only used for testing.
incAfterStateFrep :: Cell -> Bool -> Ch -> Acc Grid -> Acc Frep -> Acc Frep
incAfterStateFrep cell eIsEnd ch grid frep = incFrep
  where
    chs = use $ fromList (Z:.1) [ch]
    freps = incAfterStateFreps (constant cell) (constant eIsEnd) chs grid frep
    incFrep = slice freps (lift (Z :. (0 ::Exp Int) :. All :. All :. All)) :: Acc Frep


-- | Given a grid, its feature representation frep,
-- | and a set of actions specified by cell, event type and a list of channels,
-- | derive feature representations for the afterstates of grid incrementally.
-- | The shape of the resulting array is (Z :. n_chs :. rOWS :. cOLS :. cHANNELS + 1).
incAfterStateFreps :: Exp Cell -> Exp Bool -> Acc Chs -> Acc Grid -> Acc Frep -> Acc Freps
incAfterStateFreps cell@(T2 r c) eIsEnd chs grid frep = nInuse ++ nElig
  where
    -- Here are some tips on grokking the code below
    -- 1: Understand how a feature representation is built from scratch (see `featureRep`)
    -- 2: Understand what afterstates are (see `afterstates`)
    -- 3: Understand how to construct the frep of the next grid state,
    -- incrementally from the current frep, given an grid, an event and
    -- an action in response to that event.
    -- See the equations in the thesis `Contributions .. ` linked in the Readme.
    -- Also see the `incremental_freps` function in `gridfuncs_numba.py` in the
    -- Python DCA project.
    ---------------------------------------------------------------------------------
    -- If an action triggers a change in a feature, the value of the feature will change by
    -- either (+1) or (-1) depending on both the event and feature type.
    diff = cond eIsEnd (-1) 1
    zgrid = permute const grid (\chSh -> index3 r c (chs ! chSh)) (fill (shape chs) (lift False))
    grid' = acond eIsEnd zgrid grid :: Acc Grid

    -- For each possible action, the feature rep. of the afterstate
    afreps = replicate (lift (Z :. length chs :. All :. All :. All)) frep :: Acc Freps
    neighs2 = getNeighborhoodAcc' 2 cell (constant True) :: Acc (Array DIM1 Cell)
    neighs4 = getNeighborhoodAcc' 4 cell (constant False)

    -- Compute the inuse features
    nIinit = init afreps
    nInuse = permute (+) nIinit inuseIComb $ fill (index2 (length chs) (length neighs4)) diff
    inuseIComb :: Exp DIM2 -> Exp DIM4
    inuseIComb (D2 chIx neighIx) = lift (Z :. chIx :. r' :. c' :. ch)
      where
        ch = chs ! index1 chIx
        T2 r' c' = neighs4 ! index1 neighIx

    -- Compute the change in eligibility feature
    eligDiff = generate (lift (Z :. length chs :. length neighs2)) mapCh
    mapCh :: Exp DIM2 -> Exp Int
    mapCh (D2 chIx neighIx) = cond eligible (-diff) 0
      where
        ch = chs ! index1 chIx
        n2 = neighs2 ! index1 neighIx
        T2 start n = getNeighborhoodOffsets 2 n2 (constant True)
        eligible = not $ fst $ iterate n orNeigh (lift (False, start))
        orNeigh :: Exp (Bool, Int) -> Exp (Bool, Int)
        orNeigh (T2 acc_nelig acc_idx) = lift (acc_nelig || neighBit, acc_idx + 1)
          where
            T2 r'' c'' = _neighs ! index1 acc_idx
            neighBit = grid' ! index3 r'' c'' ch
    -- THE FOLDING APPROACH
    -- FIRST, construct the dim1 list:
    -- allNbs = concat [nbhood 2 (r', c') True | (r', c') <- nbhood 2 (r, c) True]
    -- with corresponding segments
    -- THEN sliceNeighs allNbs grid'
    -- THEN see featureRep


    -- Compute the eligibility feature by adding the diff to the value of the
    -- feature at the last time step
    nEinit = drop (lift cHANNELS) afreps -- Remember, we're dropping along the 4th dim.
    nElig = permute (+) nEinit eligIComb eligDiff
    -- There is k entries in 'eligDiff' for each frep in 'afreps';
    -- or equivalently, for each ch in 'chs', where k is number of neighs in d2 nb.hood.
    -- Remember we're populating a an array (nEinit) with feature depth of 1, and
    -- concatting the feature depths afterwards.
    eligIComb :: Exp DIM2 -> Exp DIM4
    eligIComb (D2 chIx neighIx) = lift (Z :. chIx :. r' :. c' :. (0 :: Int))
      where
        T2 r' c' = neighs2 ! index1 neighIx
