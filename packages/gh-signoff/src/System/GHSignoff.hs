{-# LANGUAGE TemplateHaskell #-}

{- | Working with gh-signoff

A standalone library for gh-signoff operations.
-}
module System.GHSignoff where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `gh-signoff` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
ghSignoffBin :: FilePath
ghSignoffBin = $(staticWhich "gh-signoff")

-- | Create a process to run gh-signoff create with force flag
ghSignoffProcess :: String -> String -> CreateProcess
ghSignoffProcess name k = proc ghSignoffBin ["create", "-f", name <> "/" <> k]
