{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
  viewToolStatus,
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
import Lucid (HtmlT, class_, code_, div_, p_, span_, strong_, toHtml)
import TOML (TOMLError)
import Vira.Tool.Type (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)

-- | All errors that can occur when working with Attic
data AtticError
  = -- | URL parsing failed
    UrlParseError Url.ParseError
  | -- | TOML configuration error
    ConfigError TOMLError
  | -- | Attic is not configured
    NotConfigured
  | -- | No server configured for endpoint
    NoServerForEndpoint Text
  deriving stock (Show, Eq)

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData, Either TOMLError (Maybe Attic.Config.AtticConfig))
getToolData = do
  info <- liftIO Attic.Config.readAtticConfig
  let metadata =
        ToolData
          { name = "Attic"
          , description = "Self-hosted Nix binary cache server"
          , url = "https://github.com/zhaofengli/attic"
          , binPaths = one $ toText Attic.atticBin
          }
  pure (metadata, info)

{- | Create attic push process from cache URL and config

Takes the attic config result, cache URL, and path to push.
Returns either an AtticError or the CreateProcess for pushing.
-}
createPushProcess ::
  Either TOMLError (Maybe AtticConfig) ->
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

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either TOMLError (Maybe AtticConfig) -> HtmlT m ()
viewToolStatus cfg = do
  div_ [class_ "mb-3"] $ do
    case cfg of
      Left err -> do
        viraAlert_ AlertError $ do
          p_ [class_ "text-red-800 font-semibold mb-1"] "✗ Parse error"
          p_ [class_ "text-red-700 text-sm"] $ toHtml (show err :: String)
      Right Nothing -> do
        viraAlert_ AlertWarning $ do
          p_ [class_ "text-yellow-800 mb-1"] "⚠ Not configured"
          p_ [class_ "text-yellow-700 text-sm"] $ do
            "Config file not found at "
            code_ [class_ "bg-yellow-100 px-1 rounded"] "~/.config/attic/config.toml"
      Right (Just atticCfg) -> do
        viraAlert_ AlertSuccess $ do
          case atticCfg.defaultServer of
            Just defServer -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] $ do
                "✓ Default server: "
                strong_ $ toHtml defServer
            Nothing -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] "✓ Configured"

          -- Display all configured servers
          unless (Map.null atticCfg.servers) $ do
            p_ [class_ "text-green-700 text-xs mt-2 mb-1"] "Configured servers:"
            div_ [class_ "space-y-1"] $ do
              forM_ (Map.toList atticCfg.servers) $ \(serverName, serverCfg) -> do
                div_ [class_ "text-green-700 text-xs pl-2"] $ do
                  strong_ $ toHtml serverName
                  ": "
                  code_ [class_ "bg-green-100 px-1 rounded"] $ toHtml serverCfg.endpoint
                  case serverCfg.token of
                    Just _ -> span_ [class_ "ml-2 text-green-600"] "🔑"
                    Nothing -> span_ [class_ "ml-2 text-yellow-600"] "⚠ No token"
