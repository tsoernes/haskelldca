module TestBase where

import Data.Maybe (fromJust)
import Data.Word (Word64)
import Opt
import Options.Applicative
import Debug.Trace (trace)

seed :: Word64
seed = 0

-- | Default (pure) options.
-- | Will print options on import (or the failure of parsing the defaults)
popts :: Opt
popts = fromJust $ getParseResult $ trace ("\n" ++ show _optParse) _optParse
  where
    _optParse :: ParserResult Opt
    _optParse = execParserPure defaultPrefs _opts []
    _opts :: ParserInfo Opt
    _opts = info (getOpts 0 <**> helper) fullDesc
