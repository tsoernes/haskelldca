module Base where
import Data.Array.Accelerate (Acc, Array, DIM1, DIM3, DIM4)

rows = 7 :: Int
cols = 7 :: Int
channels = 70 :: Int
gridIdxs = concat [[(r, c) | c <- [0..cols-1]] | r <- [0..rows-1]]

type Cell = (Int, Int)
type GridCell = Acc (Array DIM1 Bool)
type Grid = Acc (Array DIM3 Bool)
type Grids = Acc (Array DIM4 Bool)
type Frep = Acc (Array DIM3 Int)
type Freps = Acc (Array DIM4 Int)
type Chs = Acc (Array DIM1 Int)

data EType = NEW | END | HOFF deriving (Show, Eq, Ord)

data Event = Event {
  id :: Int,
  time :: Float,
  etype :: EType,
  cell :: Cell,
  ch :: Maybe Int,
  toCell :: Maybe Cell
  }

data EventI
