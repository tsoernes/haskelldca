module TestGridneighs where

import qualified Data.Set
import Gridneighs
import Base
import Criterion.Main


main = defaultMain [
  bgroup "periphery" [ bench "A"  $ nf (map (uncurry periphery)) inp
                     , bench "B"  $ nf (map (uncurry periphery')) inp
                     ]
  ]
  where
    inp = [(d, cell) | cell <- gridIdxs, d <- [1..3]]
  
isEq :: Bool
isEq = not $ any snd [(cell, p1 d cell /= p2 d cell) | cell <- gridIdxs, d <- [1..3]]
  where
    p1 d cell = Data.Set.fromList $ periphery d cell
    p2 d cell = Data.Set.fromList $ periphery' d cell
