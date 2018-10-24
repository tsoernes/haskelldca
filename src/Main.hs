module Main where

import Data.Word ( Word64 )
import Opt ( getOpts )
import Options.Applicative ( (<**>), fullDesc, header, info, execParser, helper )
import SimRunner ( runSim )
import System.Random (randomIO)
import           Control.Monad.Reader ( runReaderT )

main :: IO ()
main = do
  seed <- randomIO :: IO Word64
  let opts =
        info
          (getOpts seed <**> helper)
          (fullDesc <>
           header "DCA - Dynamic Channel Allocation by Reinforcement Learning")
  popts <- execParser opts
  print popts
  runReaderT runSim popts

  return ()
