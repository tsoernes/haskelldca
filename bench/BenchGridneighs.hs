module BenchGridneighs where

import Gridneighs
import Base
import Criterion.Main


main = defaultMain [
  bgroup "periphery" [ bench "A"  $ nf (map (uncurry periphery)) inp
                     , bench "B"  $ nf (map (uncurry periphery)) inp
                     ]
  ]
  where
    inp = [(d, cell) | cell <- gridIdxs, d <- [1..4]]
