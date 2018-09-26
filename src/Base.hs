{-# LANGUAGE TemplateHaskell #-}
module Base where

import Data.Array.Accelerate (Acc, Array, DIM1, DIM3, DIM4)
import Control.Lens (makeLenses)

rOWS = 7 :: Int
cOLS = 7 :: Int
cHANNELS = 70 :: Int
gridIdxs = concat [[(r, c) | c <- [0..cOLS-1]] | r <- [0..rOWS-1]]

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
  _evId :: EventId,
  _evTime :: Double,
  _evType :: EType,
  _evCell :: Cell,
  _evEndCh :: Maybe Ch, -- For END events only
  _evHoffCell :: Maybe Cell -- For HOFF events only
  } deriving Eq

makeLenses ''Event

data EventKey = EventKey { ekTime :: Double
                         , ekId :: EventId
                         } deriving (Eq, Show)

-- | Used to assure that the END event of a hand-off is handled before the HOFF part
instance Ord EventKey where
  e1 `compare` e2 = case ekTime e1 `compare` ekTime e2 of
    EQ -> ekId e1 `compare` ekId e2
    x -> x

