{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
  AtticError (..),
) where

import Attic (AtticCache (..), AtticServer (..))
import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Attic.Url qualified as Url
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import TOML.Error qualified as TOML
import Vira.Tool.Type (ToolData (..))

-- | All errors that can occur when working with Attic
data AtticError
  = -- | URL parsing failed
    UrlParseError Url.ParseError
  | -- | TOML configuration error
    ConfigError TOML.TOMLError
  | -- | Attic is not configured
    NotConfigured
  | -- | No server configured for endpoint
    NoServerForEndpoint Text
  deriving stock (Show, Eq)

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either TOML.TOMLError (Maybe Attic.Config.AtticConfig)))
getToolData = do
  info <- liftIO Attic.Config.readAtticConfig
  pure
    ToolData
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = one $ toText Attic.atticBin
      , info
      }

{- | Create attic push process from cache URL and config

Takes the attic config result, cache URL, and path to push.
Returns either an AtticError or the CreateProcess for pushing.
-}
createPushProcess ::
  Either TOML.TOMLError (Maybe AtticConfig) ->
  Text ->
  FilePath ->
  Either AtticError CreateProcess
createPushProcess atticConfigResult cacheUrl path = do
  -- Parse cache URL to extract server endpoint and cache name
  (serverEndpoint, cacheName) <- first UrlParseError $ Url.parseCacheUrl cacheUrl

  -- Validate and extract config
  mConfig <- first ConfigError atticConfigResult
  config <- mConfig & maybeToRight NotConfigured

  -- Find server config that matches the endpoint
  (serverName, _) <-
    find (\(_name, serverCfg) -> serverCfg.endpoint == serverEndpoint) (Map.toList config.servers)
      & maybeToRight (NoServerForEndpoint serverEndpoint)

  -- Create the push process
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) (AtticCache cacheName) path
