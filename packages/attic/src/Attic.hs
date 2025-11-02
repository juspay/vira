{-# LANGUAGE TemplateHaskell #-}

-- | Working with [attic](https://github.com/zhaofengli/attic) cache servers
module Attic (
  atticBin,
  atticPushProcess,
  atticLoginProcess,
) where

import Attic.Types (AtticCache, AtticServer (..), AtticToken)
import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the @attic@ executable

This should be available in the PATH, thanks to Nix and @which@ library.
-}
atticBin :: FilePath
atticBin = $(staticWhich "attic")

{- | Push the given paths to the 'AtticServer' cache

NOTE: 'atticLoginProcess' should be run before this to set the access token
-}
atticPushProcess :: AtticServer -> AtticCache -> NonEmpty FilePath -> CreateProcess
atticPushProcess AtticServer {name} cacheName paths =
  proc atticBin $ ["push", toString name <> ":" <> toString cacheName] <> toList paths

{- | Saves the 'AtticToken' for the 'AtticServer'

Run this process before other attic processes.

NOTE: It is not secure to log this CLI; cf. https://github.com/zhaofengli/attic/issues/243
-}
atticLoginProcess :: AtticServer -> AtticToken -> CreateProcess
atticLoginProcess AtticServer {name, endpoint} token =
  proc atticBin ["login", toString name, toString endpoint, toString token]
