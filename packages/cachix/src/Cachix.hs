{-# LANGUAGE TemplateHaskell #-}

-- | Working with [Cachix](https://cachix.org) binary cache
module Cachix (
  cachixBin,
  cachixPushProcess,
) where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `cachix` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
cachixBin :: FilePath
cachixBin = $(staticWhich "cachix")

{- | Push the given path to the cachix cache

Creates a process that pushes a Nix store path to the specified cache.
-}
cachixPushProcess :: Text -> FilePath -> CreateProcess
cachixPushProcess cache path = proc cachixBin ["push", "-v", toString cache, path]
