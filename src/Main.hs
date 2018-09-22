module Main where

import Gridneighs
import Gridfuncs
import Control.Monad.Reader
import Base
import Opt
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