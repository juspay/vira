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

-- | Path to the `attic` executable
atticBin :: FilePath
atticBin = $(staticWhich "attic")

cachixPushProcess :: Text -> FilePath -> CreateProcess
cachixPushProcess cache path = proc cachixBin ["push", "-v", toString cache, path]

atticPushProcess :: Text -> FilePath -> CreateProcess
atticPushProcess cacheIdentifier path = proc atticBin ["push", toString cacheIdentifier, path]

atticLoginProcess :: Text -> Text -> Text -> CreateProcess
atticLoginProcess loginName cacheUrl token = proc atticBin ["login", toString loginName, toString cacheUrl, toString token]
