{-# LANGUAGE Rank2Types #-}

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
} deriving (Show)

getOpts :: Parser Opt
getOpts =
  Opt <$>
  option
    auto
    (long "call_dur" <> metavar "MINUTES" <> showDefault <> value 3.0 <>
     help "Call duration for new calls") <*>
  option
    auto
    (long "call_dur_hoff" <> metavar "MINUTES" <> showDefault <> value 3.0 <>
     help "Call duration for new calls") <*>
  option
    auto
    (long "call_rate" <> metavar "CALLS_PER_HOUR" <> showDefault <> value 200.0 <>
     help "Call arrival rate") <*>
  option
    auto
    (long "hoff_prob" <> metavar "PROBABILITY" <> showDefault <> value 0.0 <>
     help "Hand-off probability") <*>
  option
    auto
    (long "n_events" <> metavar "N" <> showDefault <> value 10000 <>
     help "Simulation duration in number of processed events") <*>
  option
    auto
    (long "log_iter" <> metavar "N" <> showDefault <> value 1000 <>
     help "How often to show call blocking probability and stats") <*>
  option
    auto
    (long "learning_rate" <> metavar "alpha_net" <> showDefault <> value 2.52e-6) <*>
  option
    auto
    (long "learning_rate_avg" <> metavar "alpha_avg" <> showDefault <>
     value 0.06 <>
     help "") <*>
  option
    auto
    (long "learning_rate_grad" <> metavar "alpha_grad" <> showDefault <>
     value 5e-6 <>
     help "") <*>
  switch (long "verify_reuse_constraint") <*>
  option bendOpt (long "backend" <> value Interpreter <> showDefault <> help bendOptStr)

bendOptStr :: String
bendOptStr = "Accepted backends are 'interp' for 'Interpreter' and 'cpu' for 'LLVM.Native'."

bendOpt :: ReadM Backend
bendOpt = str >>= \s -> case map toLower s of
    "interp" -> return Interpreter
    "cpu" -> return CPU
    _ -> readerError bendOptStr