module TestBase where

import Base
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Opt
import Options.Applicative
import Test.Hspec
import Debug.Trace (trace)

seed :: Word64
seed = 0

-- Get default (pure) options and print them (or the failure of parsing defaults)
_opts = info (getOpts <**> helper) fullDesc
res = execParserPure defaultPrefs _opts []
popts = fromJust $ getParseResult $ trace ("\n" ++ show res) res
