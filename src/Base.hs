{-# LANGUAGE TemplateHaskell #-}

module Base where

import Control.Lens (makeLenses)
import Data.Array.Accelerate
  ( (:.)((:.))
  , Acc
  , Array
  , DIM1
  , DIM3
  , DIM4
  , Exp
  , Lift(lift)
  , Scalar
  , Z(Z)
  , constant
  , fill
  )
import qualified Data.Array.Accelerate as A
import Prelude as P

-- Grid dimensions and (system-wide) bandwidth.
rOWS, cOLS, cHANNELS :: Int
rOWS = 7

cOLS = 7

cHANNELS = 70

gridIdxs :: [Cell]
gridIdxs = concat [[(r, c) | c <- [0 .. cOLS - 1]] | r <- [0 .. rOWS - 1]]

gridIdxsExp :: Acc (Array DIM1 Cell)
gridIdxsExp = A.use $ A.fromList (Z :. length gridIdxs) gridIdxs

-- | For a given cell, which channels are in use
type GridCell = Array DIM1 Bool

-- | The grid keeps track of which channels are in use where.
-- | If `grid[row][col][ch]==True`, then channel `ch` is currently
-- | in use at the cell with coordinates row, col.
-- | A channel can be assigned set from False to True in a cell iff
-- | there are no neighbors with hexagonal distance of 2 or less
-- | that already use the channel.
type Grid = Array DIM3 Bool

type Grids = Array DIM4 Bool

-- | Feature representation of a grid
type Frep = Array DIM3 Int

type Freps = Array DIM4 Int

-- | A set of channels, each in the range [0,1,...,cHANNELS.
type Chs = Array DIM1 Int

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
  | END Ch
        (Maybe Cell)
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

-- | If the event is an END event and is scheduled to hand off.
willHoff :: EType -> Bool
willHoff (END _ (Just _)) = True
willHoff _ = False

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

-- All arrays and scalars that form a part of the state and that should be run with Accelerate
-- are put in tuple aliases. Allows for "Acc SomeState -> Acc SomeState" expressions w/o
-- deriving "instance Arrays SomeState".
-- (wNet, wGradCorr, avgReward)
type Agent = (Array DIM1 Float, Array DIM1 Float, Scalar Float)

-- Useful for unlifting the type above into its parts
type AccAgent = (Acc (Array DIM1 Float), Acc (Array DIM1 Float), Acc (Scalar Float))

agWNet :: Agent -> Array DIM1 Float
agWNet (n, _, _) = n

agWGradCorr :: Agent -> Array DIM1 Float
agWGradCorr (_, g, _) = g

agAvgReward :: Agent -> Scalar Float
agAvgReward (_, _, a) = a

data Backend
  = Interpreter
  | CPU
  deriving (Show)
