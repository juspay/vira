{-# LANGUAGE TemplateHaskell #-}

-- | Working with cachix
module Vira.Lib.Cachix where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `cachix` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
cachixBin :: FilePath
cachixBin = $(staticWhich "cachix")

cachixPushProcess :: Text -> FilePath -> CreateProcess
cachixPushProcess cache path = proc cachixBin ["push", "-v", toString cache, path]
