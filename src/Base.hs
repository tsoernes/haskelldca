{-# LANGUAGE TemplateHaskell #-}

module Base where

import Control.Arrow ( (***) )
import Control.Lens ( makeLenses )
import Data.Array.Accelerate ( fill, constant, (:.)((:.)), Array, DIM1, DIM3, DIM4, Z(Z), Acc, Exp, Lift(lift) )
import Prelude as P

rOWS = 7 :: Int

cOLS = 7 :: Int

cHANNELS = 70 :: Int

gridIdxs :: [Cell]
gridIdxs = concat [[(r, c) | c <- [0 .. cOLS - 1]] | r <- [0 .. rOWS - 1]]

gridIdxsExp :: [(Exp Int, Exp Int)]
gridIdxsExp = P.map (lift *** lift) gridIdxs

type GridCell = Acc (Array DIM1 Bool)

type Grid = Acc (Array DIM3 Bool)

type Grids = Acc (Array DIM4 Bool)

type Frep = Acc (Array DIM3 Int)

type Freps = Acc (Array DIM4 Int)

type Chs = Acc (Array DIM1 Int)

type Ch = Int

type Cell = (Int, Int)

data EType
  = NEW
  | END Ch
        (Maybe Cell)
  -- ^ Ch: The channel currently in use to be terminated
  -- Cell: The cell to which the call will be handed off (for HOFF events only)
  | HOFF
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

data EventKey = EventKey
  { ekTime :: Double
  , ekId :: EventId
  } deriving (Eq, Show)

-- | Used to assure that the END event of a hand-off is handled before the HOFF part
instance Ord EventKey where
  e1 `compare` e2 =
    case ekTime e1 `compare` ekTime e2 of
      EQ -> ekId e1 `compare` ekId e2
      x -> x

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
