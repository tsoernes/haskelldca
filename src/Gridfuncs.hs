{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Gridfuncs where

import Control.Arrow ((&&&), (***), first)
import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native (run)
import Gridneighs
import Base
import qualified Prelude as P

mkGrid :: Grid
mkGrid = fill (constant (Z :. rOWS :. cOLS :. cHANNELS)) (lift False)

mkFrep :: Frep
mkFrep = fill (constant (Z :. rOWS :. cOLS :. cHANNELS )) 0

mkFreps :: Exp Int -> Freps
mkFreps i = fill (index4 (lift i) (lift rOWS) (lift cOLS) (lift cHANNELS)) 0

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
eligibleChs cell grid = indicesOf $ eligibleMap cell grid


-- | Return the eligible channels for the given cell. A channel is eligible if it is free at
-- | the cell and all of its neighbors with distance of 2 or less.
inuseChs :: Cell -> Grid -> Chs
inuseChs cell grid = indicesOf $ inuseMap cell grid


-- | Convert a one-hot vector (sparse repr) to a vector of indecies (dense repr)
indicesOf :: Acc (Array DIM1 Bool) -> Acc (Array DIM1 Int)
indicesOf arr = iHot
  where
    -- Pair up every element with its index (as Shape)
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
    fn frep (r,c) = permute const frep idxmap (nUsed ++ nElig)
      where
        nUsedZ = fill (constant (Z :. cHANNELS + 1)) 0 :: Acc (Vector Int)
        neighs4 = getNeighs 4 (r, c) False
        nUsed = P.foldl (\acc cell -> zipWith (+) acc (map boolToInt $ sliceCell cell grid)) nUsedZ neighs4
        nElig = reshape (constant (Z :. 1)) . boolSum $ eligibleMap (r,c) grid
        -- Traverse the 3rd dimension of the given cell
        idxmap :: Exp DIM1 -> Exp DIM3
        idxmap nsh = index3 (lift r) (lift c) (unindex1 nsh)

boolSum :: Shape sh => Acc (Array (sh :. Int) Bool) -> Acc (Scalar Int)
boolSum = sum . flatten . map boolToInt

boolSum2 :: Shape sh => Acc (Array (sh :. Int) Bool) -> Exp Int
boolSum2 = the . sum . flatten . map boolToInt

incrementalFreps :: Grid -> Frep -> Cell -> EType -> Chs -> Freps
incrementalFreps = undefined

-- | Switch bit at given cell off for END events; on for NEW events
executeAction :: Event -> Ch -> Grid -> Grid
executeAction Event{_evType, _evCell=(r, c)} toCh grid =
    let val = unit $ lift $ _evType P./= END
        setIdx _ = constant (Z :. r :. c :. toCh)
    in permute const grid setIdx val
  
-- | Create a rank-4 index from four Exp Int`s
index4
    :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
    => Exp i
    -> Exp i
    -> Exp i
    -> Exp i
    -> Exp (Z :. i :. i :. i :. i)
index4 k j i l = lift (Z :. k :. j :. i :. l)

vvMul :: Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
vvMul xs ys = fold (+) 0 ( zipWith (*) xs ys )

mvMul :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvMul mat vec =
  let Z :. rows :. _ = unlift (shape mat) :: Z :. Exp Int :. Exp Int
      vec'              = replicate (lift (Z :. rows :. All)) vec
  in
  fold (+) 0 ( zipWith (*) mat vec' )

mmMul :: Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
mmMul mat1 mat2 = undefined

outer ::
   (Num a) =>
   Acc (Vector a) -> Acc (Vector a) -> Acc (Matrix a)
outer x y =
   zipWith (*)
      (replicate (lift $ Any :. All :. length y) x)
      (replicate (lift $ Any :. length x :. All) y)

multiplyMatrixVector :: (Num a) =>
   Acc (Matrix a) ->
   Acc (Vector a) ->
   Acc (Vector a)
multiplyMatrixVector m v =
  let rows = fst . unindex2 $ shape m :: Exp Int
  in fold1 (+) $ zipWith (*) m (replicate (lift $ Any :. rows :. All) v)

multiplyMatrixMatrix ::
   (Num a) =>
   Acc (Matrix a) ->
   Acc (Matrix a) ->
   Acc (Matrix a)
multiplyMatrixMatrix x y =
  let rows = fst . unindex2 $ shape x :: Exp Int
      cols = snd . unindex2 $ shape y :: Exp Int
      a1 = replicate (lift $ Z :. All :. All :. cols) x :: Acc (Array DIM3 _)
      a2 = replicate (lift $ Z :. rows :. All :. All) y :: Acc (Array DIM3 _)
      b = zipWith (*) a1 a2 :: Acc (Array DIM3 _)
      -- c = transpose b
  -- in fold1 (+) $ c
  in undefined

(#*#) :: (Num a) =>
    Acc (Array DIM2 a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
v #*# w = let (k, m) = unlift . unindex2 $ shape v :: (Exp Int, Exp Int)
              (m', n) = unlift . unindex2 $ shape w :: (Exp Int, Exp Int)
          in generate (index2 k n) (aux v w)
          where aux :: (Num e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Exp DIM2 -> Exp e
                aux v w sh = let (i, j) = unlift $ unindex2 sh :: (Exp Int, Exp Int)
                                 v' = slice v (lift $ Z:.i:.All)
                                 w' = slice w (lift $ Z:.All:.j)
                              in the $ sum $ zipWith (*) v' w'
  

runExp :: (Elt a) => Exp a -> a
runExp expa = indexArray (run $ unit expa) Z

-- foldMax :: (Ord a, Elt a, Shape sh) => Acc (Array sh a) -> (Exp sh, Exp a)
-- foldMax arr = fold1All fn (flatten $ indexed arr)
--   where
--     fn :: (Elt sh, Ord e, Elt e) => Exp (sh, e) -> Exp (sh, e) -> Exp (sh, e)
--     fn a b = cond (snd a > snd b) a b
    
-- | Argmax plus max: Return (idx :: sh, val) pair for the largest element in the array
argpmax :: (Ord a, Elt a, Shape sh) => Acc (Array sh a) -> (Exp sh, Exp a)
argpmax arr = unlift $ the x
  where
    x = fold1All fn (flatten $ indexed arr)
    fn :: (Elt sh, Ord e, Elt e) => Exp (sh, e) -> Exp (sh, e) -> Exp (sh, e)
    fn a b = cond (snd a > snd b) a b


-- | Return (idx :: Int, val) pair for the largest element in the vector
argpmax1 :: (Ord a, Elt a) => Acc (Array DIM1 a) -> (Exp Int, Exp a)
argpmax1 = first unindex1 . argpmax

expand :: Exp Int -> Acc (Array DIM1 Int)
expand = replicate (constant (Z :. (3::Int))) . unit