module Main where

import Data.Word ( Word64 )
import Opt ( getOpts )
import Options.Applicative ( (<**>), fullDesc, header, info, execParser, helper )
import SimRunner ( runSim )

main :: IO ()
main = do
  let opts =
        info
          (getOpts <**> helper)
          (fullDesc <>
           header "DCA - Dynamic Channel Allocation by Reinforcement Learning")
  popts <- execParser opts
  print popts
  let seed = 0 :: Word64
  runSim seed popts

  return ()
