module Base where
import Data.Array.Accelerate (Acc, Array, DIM1, DIM3, DIM4)

rows = 7 :: Int
cols = 7 :: Int
channels = 70 :: Int
gridIdxs = concat [[(r, c) | c <- [0..cols-1]] | r <- [0..rows-1]]

type GridCell = Acc (Array DIM1 Bool)
type Grid = Acc (Array DIM3 Bool)
type Grids = Acc (Array DIM4 Bool)
type Frep = Acc (Array DIM3 Int)
type Freps = Acc (Array DIM4 Int)
type Chs = Acc (Array DIM1 Int)

type Ch = Int
type Cell = (Int, Int)
data EType = NEW | END | HOFF deriving (Show, Eq, Ord)
type EventId = Int

data Event = Event {
  eId :: EventId,
  time :: Double,
  etype :: EType,
  cell :: Cell,
  endCh :: Maybe Ch, -- For END events only
  hoffCell :: Maybe Cell -- For HOFF events only
  } deriving Eq

data EventKey = EventKey { ekTime :: Double
                         , ekId :: EventId
                         } deriving (Eq, Show)

-- | Used to assure that the END event of a hand-off is handled before the HOFF part
instance Ord EventKey where
  e1 `compare` e2 = case ekTime e1 `compare` ekTime e2 of
    EQ -> ekId e1 `compare` ekId e2
    x -> x

