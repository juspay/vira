{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
) where

import Attic (AtticCache (..), AtticServer (..))
import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Attic.Url qualified
import Data.Map.Strict qualified as Map
import Data.Some (Some (Some))
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import TOML.Error qualified as TOML
import Vira.Tool.Type (Tool (..), ToolData (..), ToolError (..))

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
Returns either a ToolError or the CreateProcess for pushing.
-}
createPushProcess ::
  Either TOML.TOMLError (Maybe AtticConfig) ->
  Text ->
  FilePath ->
  Either ToolError CreateProcess
createPushProcess atticConfigResult cacheUrl path = do
  -- Parse cache URL to extract server endpoint and cache name
  (serverEndpoint, cacheName) <- first (ToolError (Some Attic) . toText) $ Attic.Url.parseCacheUrl cacheUrl

  -- Validate and extract config
  mConfig <- first (\err -> ToolError (Some Attic) (toText (show err :: String))) atticConfigResult
  config <- mConfig & maybeToRight (ToolError (Some Attic) "Attic not configured. Run: attic login <name> <endpoint> <token>")

  -- Find server config that matches the endpoint
  (serverName, _) <-
    find (\(_name, serverCfg) -> serverCfg.endpoint == serverEndpoint) (Map.toList config.servers)
      & maybeToRight (ToolError (Some Attic) ("No attic server configured for " <> serverEndpoint <> ". Run: attic login <name> " <> serverEndpoint <> " <token>"))

  -- Create the push process
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) (AtticCache cacheName) path
