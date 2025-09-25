{-# LANGUAGE TemplateHaskell #-}

{- | Working with gh-signoff

A standalone library for gh-signoff operations.
-}
module GH.Signoff (ghSignoffProcess) where

import System.Info qualified as SysInfo
import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `gh-signoff` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
ghSignoffBin :: FilePath
ghSignoffBin = $(staticWhich "gh-signoff")

{- | System platform string

Computed from System.Info at runtime in Nix format (arch-os).
-}
nixSystem :: String
nixSystem = SysInfo.arch <> "-" <> SysInfo.os

-- | Create a process to run gh-signoff create with force flag and platform context
ghSignoffProcess :: String -> String -> CreateProcess
ghSignoffProcess name k =
  proc ghSignoffBin ["create", "-f", name <> "/" <> nixSystem <> "/" <> k]
