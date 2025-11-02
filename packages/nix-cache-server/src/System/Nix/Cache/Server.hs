{-# LANGUAGE DuplicateRecordFields #-}

{- | Nix binary cache server integration using nix-serve-ng

This module provides a WAI application for serving a Nix binary cache using
the <https://github.com/aristanetworks/nix-serve-ng nix-serve-ng> library.

= Usage

The typical flow is:

1. Generate or load cache signing keys using "System.Nix.Cache.Keys"
2. Call 'makeCacheServer' with the keys to create a WAI Application
3. Mount the application in your web server (e.g., at @\/cache@)

The resulting cache server exposes standard Nix cache endpoints like:

* @\/nix-cache-info@ - Cache metadata
* @\/*.narinfo@ - NAR info files
* @\/nar\/*.nar@ - NAR archives

This integrates with nix-serve-ng which handles all the Nix-specific
protocols and signing of NAR files.
-}
module System.Nix.Cache.Server (
  -- * Cache server
  makeCacheServer,
  cacheMiddleware,
) where

import Data.ByteString qualified as ByteString
import Network.Wai (Application, Middleware, pathInfo, rawPathInfo)
import Nix qualified
import NixServeNg (ApplicationOptions (..), makeApplication)
import System.Nix.Cache.Keys (SecretKey, secretKeyByteString)

{- | Create a cache server WAI Application

This function:

1. Initializes the Nix store using the @nix@ library
2. Retrieves the Nix store directory path
3. Creates a nix-serve-ng application with the provided signing key

The returned WAI 'Application' can be mounted at any path in your web server
(commonly @\/cache@). It will handle all Nix binary cache protocol requests.

The cache is configured with a hardcoded priority of 30 (lower numbers have
higher priority in Nix's cache resolution).

Example:

@
CacheKeys {secretKey, publicKey} <- ensureCacheKeys "\/path\/to\/keys"
cacheApp <- makeCacheServer secretKey
-- Mount cacheApp at \/cache in your WAI server
-- Use publicKey for UI display
@
-}
makeCacheServer :: SecretKey -> IO Application
makeCacheServer secretKey = do
  -- Initialize Nix store
  Nix.initStore

  -- Get store directory
  storePath <- Nix.getStoreDir

  -- Create application options with hardcoded priority
  let options =
        ApplicationOptions
          { priority = 30 -- Lower is higher priority
          , storeDirectory = storePath
          , secretKey = Just (secretKeyByteString secretKey)
          }

  -- Return the application
  pure $ makeApplication options

{- | Middleware to mount cache server at a path prefix

This middleware intercepts requests starting with the given path prefix
(e.g., @\/cache\/*@) and routes them to the cache application, while allowing
requests to the prefix itself (e.g., @\/cache@) to pass through to the main
application.

This is useful when you want to serve a UI page at @\/cache@ while serving
the actual cache protocol at @\/cache\/nix-cache-info@, @\/cache\/*.narinfo@, etc.

Example:

@
cacheApp <- makeCacheServer secretKey
let middleware = cacheMiddleware "cache" cacheApp
@
-}
cacheMiddleware :: Text -> Application -> Middleware
cacheMiddleware prefix cacheApp app req respond =
  case pathInfo req of
    (p : rest@(_ : _)) | p == prefix -> do
      -- Only intercept if there's at least one path segment after prefix
      -- This allows /prefix (UI page) to pass through to servant
      -- while /prefix/nix-cache-info, /prefix/*.narinfo, etc go to cache app
      let rawPath = rawPathInfo req
          prefixBytes = "/" <> encodeUtf8 prefix
          rawPath' = fromMaybe rawPath $ ByteString.stripPrefix prefixBytes rawPath
          req' = req {pathInfo = rest, rawPathInfo = rawPath'}
      cacheApp req' respond
    _ -> app req respond
