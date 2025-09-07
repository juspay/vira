{-# LANGUAGE TemplateHaskell #-}

-- | Working with cachix
module Vira.Lib.Cachix where

import IncludeEnv.TH (includeEnv)
import System.Process (CreateProcess, proc)

{- | Path to the `cachix` executable

This must be set via the VIRA_CACHIX_BIN environment variable at compile time.
-}
$(includeEnv "VIRA_CACHIX_BIN" "cachixBin")

cachixBin :: FilePath
cachixPushProcess :: Text -> FilePath -> CreateProcess
cachixPushProcess cache path = proc cachixBin ["push", "-v", toString cache, path]
