{-# LANGUAGE TypeOperators #-}
{-#LANGUAGE NamedFieldPuns#-}
{-# LANGUAGE FlexibleContexts #-}
module Gridfuncs where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gridneighs
import Base
import qualified Prelude as P

-- | One-hot map of channels in use at cell neighbors with distance of 2 or less
inuseMap :: Cell -> Grid -> GridCell
inuseMap cell grid = P.foldl1 (zipWith (||)) (P.map (`sliceCell` grid) neighs)
  where
    neighs = getNeighs 2 cell False


-- | One-hot array of eligible channels
-- TODO Can this be one inside accelerate? foldl is not accelerate code
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
eligibleChs cell grid = elig
  where
    emap = eligibleMap cell grid
    -- Pair up every element with its index
    iemap = indexed emap :: Acc (Array DIM1 (DIM1, Bool))
    -- Filter out (index, elig) pairs that are not eligible
    ielig = filter snd iemap :: Acc (Array DIM1 (DIM1, Bool), Array DIM0 Int)
    -- Keep only the indices (as ints)
    uielig = unlift ielig :: (Acc (Array DIM1 (DIM1, Bool)), Acc (Array DIM0 Int))
    puielig = P.fst uielig :: Acc (Array DIM1 (DIM1, Bool))
    elig = map (unindex1 . fst) puielig :: Acc (Array DIM1 Int)

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

-- | Given a grid 'grid' and a set of actions, the latter specified by a cell, an event type
-- | and a list of channels, return the grids ('afterstates') that would result from
-- | executing each of the actions on 'grid'
afterstates :: Grid -> Cell -> EType -> Chs -> Grids
afterstates grid (r, c) etype chs = permute const grids idxmap vals
  where
    vals = fill (index1 $ length chs) (lift (etype P./= END)) :: Acc (Array DIM1 Bool)
    grids = replicate (lift (Z :. length chs :. All :. All :. All)) grid
    idxmap :: Exp DIM1 -> Exp DIM4
    idxmap nsh = index4 (unindex1 nsh) (lift r) (lift c) (lift $ chs ! nsh)

-- | A feature representation (frep for short) of the grid. The frep is of the
-- | same spatial dimension as the grid. For a given cell, the first 'CHANNELS' features
-- | specifies how many times each of the channels is in used within a 4-cell radius,
-- | not including the cell itself. An additional feature counts the number of eligible
-- | channels in that cell.
featureRep :: Grid -> Frep
featureRep grid = P.foldl fn zeros gridIdxs
  where
    zeros = fill (constant (Z :. rows :. cols :. channels + 1)) 0
    -- Fill in 1 cell at a time
    fn :: Frep -> Cell -> Frep
    fn frep (r,c) = permute const frep idxmap (nUsed ++ nElig)
      where
        nUsedZ = fill (constant (Z :. channels + 1)) 0 :: Acc (Vector Int)
        neighs4 = getNeighs 4 (r, c) False
        nUsed = P.foldl (\acc cell -> zipWith (+) acc (map boolToInt $ sliceCell cell grid)) nUsedZ neighs4
        nElig = reshape (constant (Z :. 1)) . sum . map boolToInt $ eligibleMap (r,c) grid
        -- Traverse the 3rd dimension of the given cell
        idxmap :: Exp DIM1 -> Exp DIM3
        idxmap nsh = index3 (lift r) (lift c) (unindex1 nsh)


incrementalFreps :: Grid -> Frep -> Cell -> EType -> Chs -> Freps
incrementalFreps = undefined

-- | Create a rank-4 index from four Exp Int`s
index4
    :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
    => Exp i
    -> Exp i
    -> Exp i
    -> Exp i
    -> Exp (Z :. i :. i :. i :. i)
index4 k j i l = lift (Z :. k :. j :. i :. l)


-- | Switch bit at given cell off for END events; on for NEW events
executeAction :: Event -> Ch -> Grid -> Grid
executeAction Event{etype, cell=(r, c)} toCh grid =
    let val = unit $ lift $ etype P./= END
        setIdx _ = constant (Z :. r :. c :. toCh)
    in permute const grid setIdx val