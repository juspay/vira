{-# LANGUAGE TemplateHaskell #-}

-- | Working with cachix
module Vira.Lib.Attic where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `attic` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
atticBin :: FilePath
atticBin = $(staticWhich "attic")

atticPushProcess :: Text -> Text -> FilePath -> CreateProcess
atticPushProcess serverName cacheName path = proc atticBin ["push", toString serverName <> ":" <> toString cacheName, path]

{- | Saves the access token for the attic server

Run this process before other attic processes.

TODO: Remove after https://github.com/zhaofengli/attic/issues/243 is resolved to provide stateless access
-}
atticLoginProcess :: Text -> Text -> Text -> CreateProcess
atticLoginProcess serverName serverUrl token = proc atticBin ["login", toString serverName, toString serverUrl, toString token]
