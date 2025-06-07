{-# LANGUAGE TemplateHaskell #-}

-- | Working with [attic](https://github.com/zhaofengli/attic) cache servers
module Vira.Lib.Attic where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

-- | Reference to a self-hosted attic server
data AtticServer = AtticServer
  { serverName :: Text
  , serverUrl :: Text
  }
  deriving stock (Eq, Show)

-- | An attic login token
newtype AtticToken = AtticToken {unAtticToken :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToString)

-- | An attic cache name
newtype AtticCache = AtticCache {unAtticCache :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToString)

{- | Path to the `attic` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
atticBin :: FilePath
atticBin = $(staticWhich "attic")

{- | Push the given path to the attic server cache

NOTE: `atticLoginProcess` should be run before this to set the access token
-}
atticPushProcess :: AtticServer -> AtticCache -> FilePath -> CreateProcess
atticPushProcess AtticServer {serverName} cacheName path =
  proc atticBin ["push", toString serverName <> ":" <> toString cacheName, path]

{- | Saves the access token for the attic server

Run this process before other attic processes.

TODO: Remove after https://github.com/zhaofengli/attic/issues/243 is resolved to provide stateless access
-}
atticLoginProcess :: AtticServer -> AtticToken -> CreateProcess
atticLoginProcess AtticServer {serverName, serverUrl} token =
  proc atticBin ["login", toString serverName, toString serverUrl, toString token]
