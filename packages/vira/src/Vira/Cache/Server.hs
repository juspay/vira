{-# LANGUAGE DuplicateRecordFields #-}

{- | Nix binary cache server integration

Provides integration with nix-serve-ng to expose a local Nix store as a binary cache.
-}
module Vira.Cache.Server (
  -- * Cache server
  makeCacheApplication,

  -- * Configuration
  CachePriority,
  defaultCachePriority,
) where

import Network.Wai (Application)
import Nix qualified
import NixServeNg (ApplicationOptions (..), makeApplication)
import Vira.Cache.Keys (CacheKeys (..), PublicKey, ensureCacheKeys, secretKeyByteString)

-- | Cache priority (lower is higher priority)
newtype CachePriority = CachePriority Integer
  deriving stock (Show)
  deriving newtype (Num)

-- | Default cache priority (lower is higher priority)
defaultCachePriority :: CachePriority
defaultCachePriority = 30

{- | Create a cache server WAI Application and public key

This function:
1. Initializes the Nix store
2. Ensures cache signing keys exist (generates if needed)
3. Reads the secret key for signing
4. Creates and returns a WAI Application serving the cache
5. Returns PublicKey for UI display

The returned Application can be mounted at a sub-path (e.g., /cache)
in the main Vira web server.
-}
makeCacheApplication :: FilePath -> CachePriority -> IO (Application, PublicKey)
makeCacheApplication stateDir (CachePriority priority) = do
  -- Initialize Nix store
  Nix.initStore

  -- Get store directory
  storePath <- Nix.getStoreDir

  -- Ensure cache keys exist and read them
  let cacheKeysDir = stateDir <> "/cache-keys"
  CacheKeys {secretKey, publicKey} <- ensureCacheKeys cacheKeysDir

  -- Create application options
  let options =
        ApplicationOptions
          { priority
          , storeDirectory = storePath
          , secretKey = Just (secretKeyByteString secretKey)
          }

  -- Return both the application and public key
  let app = makeApplication options
  pure (app, publicKey)
