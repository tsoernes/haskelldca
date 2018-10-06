module TestBase where

import Base
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Opt
import Options.Applicative
import Test.Hspec

seed :: Word64
seed = 0

_opts = info (getOpts <**> helper) fullDesc
-- Default (pure) options
popts = fromJust $ getParseResult $ execParserPure defaultPrefs _opts [""]
