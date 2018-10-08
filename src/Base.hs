{-# LANGUAGE TemplateHaskell #-}

module Base where

import Control.Arrow ( (***) )
import Control.Lens ( makeLenses )
import Data.Array.Accelerate ( fill, constant, (:.)((:.)), Array, DIM1, DIM3, DIM4, Z(Z), Acc, Exp, Lift(lift) )
import Prelude as P

-- Grid dimensions and (system-wide) bandwidth.
rOWS, cOLS, cHANNELS :: Int
rOWS = 7
cOLS = 7
cHANNELS = 70

gridIdxs :: [Cell]
gridIdxs = concat [[(r, c) | c <- [0 .. cOLS - 1]] | r <- [0 .. rOWS - 1]]

gridIdxsExp :: [(Exp Int, Exp Int)]
gridIdxsExp = P.map (lift *** lift) gridIdxs

-- | For a given cell, which channels are in use
type GridCell = Acc (Array DIM1 Bool)

-- | The grid keeps track of which channels are in use where.
-- | If `grid[row][col][ch]==True`, then channel `ch` is currently
-- | in use at the cell with coordinates (row, col).
-- | A channel can be assigned (set from False to True) in a cell iff
-- | there are no neighbors with hexagonal distance of 2 or less
-- | that already use the channel.
type Grid = Acc (Array DIM3 Bool)

type Grids = Acc (Array DIM4 Bool)

-- | Feature representation of a grid
type Frep = Acc (Array DIM3 Int)

type Freps = Acc (Array DIM4 Int)

-- | A set of channels, each in the range [0..cHANNELS)
type Chs = Acc (Array DIM1 Int)

-- NOTE this type alias is perhaps a bit deceptive given that the plural form is
-- an Acc expression
type Ch = Int

-- | A `cell` is a position on the grid given as (row, column).
-- | In a real-world network, a cell is a geographic area (which we approximate as a hexagon)
-- | with one Base Station that serves all mobile callers in that area.
type Cell = (Int, Int)

data EType
  = NEW
  -- ^ A regular call service request. The caller will want to be assigned a channel to use.
  | END Ch (Maybe Cell)
  -- ^ END events happen when a channel currently in use in the cell of the caller will be freed.
  -- This occurs when the caller terminates the call, or the caller chooses to move
  -- to a neighboring cell (a hand-off).
  -- Ch: The channel currently in use to be terminated.
  -- Cell: The cell to which the call will be handed off (only applicable to hand-offs,
  -- where the END event is immediately followed by a HOFF event).
  | HOFF
  -- ^ A call service request which is a continuation of a call in a neighboring cell.
  -- The call need not continue on the same channel. Servicing hand-off arrivals
  -- is deemed more important than regular NEW calls because people don't like
  -- having their conversations cut. In this simulator, a call is never handed off twice.
  deriving (Show, Eq, Ord)

isEnd :: EType -> Bool
isEnd NEW = False
isEnd (END _ _) = True
isEnd HOFF = False

isHoff :: EType -> Bool
isHoff NEW = False
isHoff (END _ _) = False
isHoff HOFF = True

hoffCell :: EType -> Maybe Cell
hoffCell (END _ mbc) = mbc
hoffCell _ = Nothing

type EventId = Int

data Event = Event
  { _evTime :: Double -- Time when event takes place
  , _evType :: EType
  , _evCell :: Cell
  } deriving (Eq, Show)

makeLenses ''Event

data Agent = Agent
  { _avgReward :: Exp Float
  , _wNet :: Acc (Array DIM1 Float)
  , _wGradCorr :: Acc (Array DIM1 Float)
  }

makeLenses ''Agent

mkAgent :: Agent
mkAgent = Agent 0.0 mk mk
  where
    mk = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1))) 0.0

data Backend
  = Interpreter
  | CPU
  deriving (Show)
