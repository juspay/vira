{-# LANGUAGE TemplateHaskell #-}

-- | Working with gh-signoff
module Vira.Lib.GHSignoff where

import IncludeEnv.TH (includeEnv)
import System.Process (CreateProcess, proc)

{- | Path to the `gh-signoff` executable

This must be set via the VIRA_GH_SIGNOFF_BIN environment variable at compile time.
-}
$(includeEnv "VIRA_GH_SIGNOFF_BIN" "ghSignoffBin")

ghSignoffBin :: FilePath

-- | Create a process to run gh-signoff create
ghSignoffProcess :: CreateProcess
ghSignoffProcess = proc ghSignoffBin []
