module Gridfuncs where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Gridneighs
import Base
import qualified Prelude as P

-- One-hot map of channels in use at cell neighbors with distance of 2 or less
inuseMap :: Cell -> Grid -> GridCell
inuseMap cell grid = P.foldl1 (zipWith (||)) (P.map (`sliceCell` grid) neighs)
  where
    neighs = getNeighs 2 cell False


-- One-hot array of eligible channels
-- TODO Can this be one inside accelerate? foldl is not accelerate code
eligibleMap :: Cell -> Grid -> GridCell
eligibleMap cell grid = map not notEmap
  where
    neighs = getNeighs 2 cell False
    -- One-hot vector where 'true' means that a channel is not eligible
    notEmap = P.foldl bitor (sliceCell cell grid) neighs
    bitor :: GridCell -> Cell -> GridCell
    bitor gc cell' = zipWith (||) gc (sliceCell cell' grid)

sliceCell :: Cell -> Grid -> GridCell
sliceCell (r, c) grid = slice grid (constant (Z :. r :. c :. All))


-- Return the eligible channels for the given cell. A channel is eligible if it is free at
-- the cell and all of its neighbors with distance of 2 or less.
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

-- Feature representation
featureRep :: Grid -> Frep
featureRep grid = P.foldl fn zeros gridIdxs
  where
    zeros = fill (constant (Z :. rows :. cols :. channels + 1)) 0
    fn :: Frep -> Cell -> Frep
    fn frep (r,c) = permute const frep indcomb gridCell
      where
        -- TODO This is where to create the vector of length CHANNELS
        gridCell = undefined
        -- Traverse the 3rd dimension of the given cell
        indcomb :: Exp DIM1 -> Exp DIM3
        indcomb nsh = index3 (lift r) (lift c) (unindex1 nsh)



incrementalFreps :: Grid -> Frep -> Cell -> EType -> Chs -> Freps
incrementalFreps = undefined
