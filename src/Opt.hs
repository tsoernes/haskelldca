module Opt where

import Data.Char (toLower)
import Data.Semigroup ((<>))
import Options.Applicative
import Base


data Opt = Opt
  { callDurNew :: Double
  , callDurHoff :: Double
  , callRate :: Double
  , hoffProb :: Double
  , nEvents :: Int
  , logIter :: Int
  , alphaNet :: Float
  , alphaAvg :: Float
  , alphaGrad :: Float
  , verifyReuseConstraint :: Bool
  , backend :: Backend
  , minLoss :: Float
} deriving (Show)


getOpts :: Parser Opt
getOpts = Opt <$>
  option auto
    (long "call_dur" <> metavar "MINUTES" <> showDefault
     <> value 3.0
     <> help "Call duration for new calls") <*>
  option auto
    (long "call_dur_hoff" <> metavar "MINUTES" <> showDefault
     <> value 1.0
     <> help "Call duration for handed-off calls") <*>
  option auto
    (long "call_rate" <> metavar "PER_HOUR" <> showDefault
     <> value 200.0
     <> help "Call arrival rate (new calls)") <*>
  option auto
    (long "hoff_prob" <> metavar "PROBABILITY" <> showDefault
     <> value 0.0
     <> help "Hand-off probability") <*>
  option auto
    (long "n_events" <> metavar "N" <> showDefault
     <> value 10000
     <> help "Simulation duration, in number of processed events") <*>
  option auto
    (long "log_iter" <> metavar "N" <> showDefault
     <> value 1000
     <> help "How often to show call blocking probability and other run time statistics") <*>
  option auto
    (long "learning_rate" <> metavar "F" <> showDefault
     <> value 2.52e-6
     <> help "For neural net, i.e. state value update") <*>
  option auto
    (long "learning_rate_avg" <> metavar "F" <> showDefault
     <> value 0.06
     <> help "Learning rate for average reward estimate") <*>
  option auto
    (long "learning_rate_grad" <> metavar "F"  <> showDefault
     <> value 5e-6
     <> help "Learning rate for gradient corrections") <*>
  switch (long "verify_reuse_constraint") <*>
  option bkendOpt
    (long "backend" <> showDefault
     <> value Interpreter
     <> help bkendOptStr)  <*>
  option auto
    (long "min_loss" <> showDefault
     <> value 1e-5
     <> help "Quit if loss goes below given absolute value. Set to 0 to disable.")

bkendOptStr :: String
bkendOptStr = "Accepted backends are 'interp' for 'Interpreter' and 'cpu' for 'LLVM.Native'."

bkendOpt :: ReadM Backend
bkendOpt = str >>= \s -> case map toLower s of
    "interp" -> return Interpreter
    "cpu" -> return CPU
    _ -> readerError bkendOptStr