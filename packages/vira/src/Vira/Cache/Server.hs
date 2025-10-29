{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Nix binary cache server integration

Provides integration with nix-serve-ng to expose a local Nix store as a binary cache.
-}
module Vira.Cache.Server (
  -- * Cache server
  makeCacheApplication,
  CacheConfig (..),
) where

import Network.Wai (Application)
import Nix qualified
import NixServeNg (ApplicationOptions (..), makeApplication)
import Vira.Cache.Keys (ensureCacheKeys, readSecretKey)

-- | Configuration for the cache server
data CacheConfig = CacheConfig
  { cacheStateDir :: FilePath
  -- ^ Directory where cache keys are stored
  , cachePriority :: Integer
  -- ^ Cache priority (lower is higher priority)
  }

{- | Create a cache server WAI Application

This function:
1. Initializes the Nix store
2. Ensures cache signing keys exist (generates if needed)
3. Reads the secret key for signing
4. Creates and returns a WAI Application serving the cache

The returned Application can be mounted at a sub-path (e.g., /cache)
in the main Vira web server.
-}
makeCacheApplication :: CacheConfig -> IO Application
makeCacheApplication config = do
  -- Initialize Nix store
  Nix.initStore

  -- Get store directory
  storeDir <- Nix.getStoreDir

  -- Ensure cache keys exist
  let cacheKeysDir = config.cacheStateDir <> "/cache-keys"
  (secretKeyPath, _publicKeyPath) <- ensureCacheKeys cacheKeysDir

  -- Read secret key
  secretKey <- readSecretKey secretKeyPath

  -- Create application options
  let options =
        ApplicationOptions
          { priority = config.cachePriority
          , storeDirectory = storeDir
          , secretKey = Just secretKey
          }

  -- Return the nix-serve-ng application
  pure $ makeApplication options
