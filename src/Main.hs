module Main where

import Base
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Word (Word64)
import Gridfuncs
import Gridneighs
import Opt
import Options.Applicative
import Scratch
import Simulator
import Utils

main :: IO ()
main = do
  let opts =
        info
          (getOpts <**> helper)
          (fullDesc <>
           header "DCA - Dynamic Channel Allocation by Reinforcement Learning")
  popts <- execParser opts
  let seed = 0 :: Word64
  runSim seed popts
  return ()