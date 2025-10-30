{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Nix binary cache server integration

Provides integration with nix-serve-ng to expose a local Nix store as a binary cache.
-}
module Vira.Cache.Server (
  -- * Cache server
  makeCacheApplication,
  CacheConfig (..),

  -- * Cache info
  CacheInfo (..),

  -- * Configuration
  defaultCachePriority,
) where

import Network.Wai (Application)
import Nix qualified
import NixServeNg (ApplicationOptions (..), makeApplication)
import Vira.Cache.Keys (ensureCacheKeys, readPublicKey, readSecretKey)

-- | Configuration for the cache server
data CacheConfig = CacheConfig
  { cacheStateDir :: FilePath
  -- ^ Directory where cache keys are stored
  , cachePriority :: Integer
  -- ^ Cache priority (lower is higher priority)
  }

-- | Information about the cache server for display in UI
data CacheInfo = CacheInfo
  { publicKey :: Text
  -- ^ Public key for users to add to their nix.conf
  , publicKeyPath :: FilePath
  -- ^ Path to the public key file (for display only)
  }

-- | Default cache priority (lower is higher priority)
defaultCachePriority :: Integer
defaultCachePriority = 30

{- | Create a cache server WAI Application and CacheInfo

This function:
1. Initializes the Nix store
2. Ensures cache signing keys exist (generates if needed)
3. Reads the secret key for signing
4. Creates and returns a WAI Application serving the cache
5. Returns CacheInfo for UI display

The returned Application can be mounted at a sub-path (e.g., /cache)
in the main Vira web server.
-}
makeCacheApplication :: CacheConfig -> IO (Application, CacheInfo)
makeCacheApplication config = do
  -- Initialize Nix store
  Nix.initStore

  -- Get store directory
  storeDir <- Nix.getStoreDir

  -- Ensure cache keys exist
  let cacheKeysDir = config.cacheStateDir <> "/cache-keys"
  (secretKeyPath, publicKeyPath) <- ensureCacheKeys cacheKeysDir

  -- Read keys
  secretKey <- readSecretKey secretKeyPath
  publicKey <- readPublicKey publicKeyPath

  -- Create application options
  let options =
        ApplicationOptions
          { priority = config.cachePriority
          , storeDirectory = storeDir
          , secretKey = Just secretKey
          }

  -- Return both the application and cache info
  let app = makeApplication options
      info = CacheInfo {publicKey, publicKeyPath}
  pure (app, info)
