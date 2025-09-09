{-# LANGUAGE TemplateHaskell #-}

-- | Working with Omnix
module Vira.Lib.Omnix where

import IncludeEnv.TH (includeEnv)
import System.Process (CreateProcess, proc)

{- | Path to the `omnix` executable

This must be set via the VIRA_OMNIX_BIN environment variable at compile time.
-}
$(includeEnv "VIRA_OMNIX_BIN" "omnixBin")

omnixBin :: FilePath
omnixCiProcess :: [String] -> CreateProcess
omnixCiProcess extraArgs = proc omnixBin (["ci", "run", "-d"] ++ extraArgs)
