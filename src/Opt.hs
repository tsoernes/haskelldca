module Opt where

import Data.Char (toLower)
import Data.Semigroup ((<>))
import Options.Applicative
import Base
import Data.Word


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
  , verifyNEligBounds :: Bool
  , backend :: Backend
  , minLoss :: Float
  , rngSeed :: Word64
} deriving (Show)


getOpts :: Word64 -> Parser Opt
getOpts rand = Opt <$>
  option auto
    (long "call_dur" <> metavar "MINUTES" <> showDefault
     <> value 3.0
     <> help "Call duration for new calls.") <*>
  option auto
    (long "call_dur_hoff" <> metavar "MINUTES" <> showDefault
     <> value 1.0
     <> help "Call duration for handed-off calls.") <*>
  option auto
    (long "call_rate" <> metavar "PER_HOUR" <> showDefault
     <> value 200.0
     <> help "Call arrival rate (new calls).") <*>
  option auto
    (long "hoff_prob" <> metavar "PROBABILITY" <> showDefault
     <> value 0.0
     <> help "Hand-off probability. Set to 0 to disable hand-offs.") <*>
  option auto
    (long "n_events" <> metavar "N" <> showDefault
     <> value 10000
     <> help "Simulation duration, in number of processed events.") <*>
  option auto
    (long "log_iter" <> metavar "N" <> showDefault
     <> value 1000
     <> help "How often to show run time statistics such as call blocking probability.") <*>
  option auto
    (long "learning_rate" <> metavar "F" <> showDefault
     <> value 2.52e-6
     <> help "For neural net, i.e. state value update.") <*>
  option auto
    (long "learning_rate_avg" <> metavar "F" <> showDefault
     <> value 0.06
     <> help "Learning rate for the average reward estimate.") <*>
  option auto
    (long "learning_rate_grad" <> metavar "F"  <> showDefault
     <> value 5e-6
     <> help "Learning rate for gradient correction.") <*>
  switch (long "verify_reuse_constraint") <*>
  switch (long "verify_nelig_bounds") <*>
  option bkendOpt
    (long "backend" <> showDefault
     <> value Interpreter
     <> help bkendOptStr)  <*>
  option auto
    (long "min_loss" <> metavar "F"  <> showDefault
     <> value 0
     <> help "Abort simulation if loss goes below given absolute value. Set to 0 to disable.") <*>
  seedParser rand


seedParser :: Word64 -> Parser Word64
seedParser rand = (\b -> if b then 0 else rand) <$>
  switch (long "fixed_rng" <> help "Use a fixed (at 0) seed for the RNG.\
                                  \ If this switch is not enabled, the seed is selected at random.")


bkendOptStr :: String
bkendOptStr = "Accepted backends are 'interp' for 'Interpreter' and 'cpu' for 'LLVM.Native'.\
              \The interpreter yields better error messages."


bkendOpt :: ReadM Backend
bkendOpt = str >>= \s -> case map toLower s of
    "interp" -> return Interpreter
    "cpu" -> return CPU
    _ -> readerError bkendOptStr
