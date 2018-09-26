module Main where

import Control.Monad.Reader
import Opt
import Gridneighs
import Gridfuncs
import Base
import Scratch
import Simulator
import Options.Applicative

main :: IO ()
main = runReader printAlphas =<< execParser opts
  where
    opts = info (getOpts <**> helper)
      ( fullDesc
     <> header "DCA - Dynamic Channel Allocation by Reinforcement Learning" )

printAlphas :: Reader Opt (IO())
printAlphas = do
  alpha <- asks alphaNet
  return $ print alpha

simulate :: Reader Opt (Double, Double)
simulate = return (0.0, 0.0)