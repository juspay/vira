-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
) where

import Attic (AtticCache (..), AtticServer (..))
import Attic qualified
import Attic.Config (AtticConfig (..))
import Attic.Config qualified
import Attic.Url qualified
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import TOML.Error qualified as TOML
import Vira.Tool.Type (ToolData (..))

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
Returns either an error message or the CreateProcess for pushing.
-}
createPushProcess ::
  Either TOML.TOMLError (Maybe AtticConfig) ->
  Text ->
  FilePath ->
  Either String CreateProcess
createPushProcess atticConfigResult cacheUrl path = do
  -- Parse cache URL to extract server endpoint and cache name
  (serverEndpoint, cacheName) <- Attic.Url.parseCacheUrl cacheUrl

  -- Validate and extract config
  mConfig <- first show atticConfigResult
  config <- mConfig & maybeToRight "Attic not configured. Run: attic login <name> <endpoint> <token>"

  -- Find server config that matches the endpoint
  (serverName, _) <-
    find (\(_name, serverCfg) -> serverCfg . endpoint == serverEndpoint) (Map.toList config . servers)
      & maybeToRight ("No attic server configured for " <> toString serverEndpoint <> ". Run: attic login <name> " <> toString serverEndpoint <> " <token>")

  -- Create the push process
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) (AtticCache cacheName) path
