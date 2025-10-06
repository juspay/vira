{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
  viewToolStatus,
  SetupError (..),
  AtticError (..),
) where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified as Url
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import Lucid (HtmlT, class_, code_, div_, p_, span_, strong_, toHtml)
import TOML (TOMLError)
import Vira.Tool.Type.ToolData (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)

-- | Configuration and setup errors
data SetupError
  = -- | TOML configuration parse error
    ParseError TOMLError
  | -- | Attic is not configured
    NotConfigured
  | -- | No server configured for endpoint
    NoServerForEndpoint AtticServerEndpoint
  | -- | Server configured but no authentication token
    NoToken Text
  deriving stock (Show, Eq)

-- | All errors that can occur when working with Attic
data AtticError
  = -- | URL parsing failed
    UrlParseError Url.ParseError
  | -- | Configuration/setup error
    SetupError SetupError
  deriving stock (Show, Eq)

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either SetupError AtticConfig))
getToolData = do
  configResult <- liftIO Attic.Config.readAtticConfig
  let status = validateConfig configResult
  pure
    ToolData
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = one $ toText Attic.atticBin
      , status = status
      }

-- | Validate attic config and check for missing tokens
validateConfig :: Either TOMLError (Maybe AtticConfig) -> Either SetupError AtticConfig
validateConfig configResult = do
  mConfig <- first ParseError configResult
  config <- mConfig & maybeToRight NotConfigured

  -- Check if any server is missing a token
  case find (isNothing . (.token) . snd) (Map.toList config.servers) of
    Just (serverName, _) -> Left (NoToken serverName)
    Nothing -> Right config

{- | Create attic push process from cache URL and config

Takes the attic config result, cache URL, and path to push.
Returns either an AtticError or the CreateProcess for pushing.
-}
createPushProcess ::
  Either SetupError AtticConfig ->
  Text ->
  FilePath ->
  Either AtticError CreateProcess
createPushProcess configResult cacheUrl path = do
  -- Parse cache URL to extract server endpoint and cache name
  (serverEndpoint, cacheName) <- first UrlParseError $ Url.parseCacheUrl cacheUrl

  -- Extract config
  config <- first SetupError configResult

  -- Find server config that matches the endpoint
  (serverName, _serverCfg) <-
    find (\(_name, serverCfg) -> serverCfg.endpoint == serverEndpoint) (Map.toList config.servers)
      & maybeToRight (SetupError (NoServerForEndpoint serverEndpoint))

  -- Create the push process (token validation already done in getToolData)
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) cacheName path

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either SetupError AtticConfig -> HtmlT m ()
viewToolStatus result = do
  div_ [class_ "mb-3"] $ do
    case result of
      Left setupErr -> case setupErr of
        ParseError err -> do
          viraAlert_ AlertError $ do
            p_ [class_ "text-red-800 font-semibold mb-1"] "✗ Parse error"
            p_ [class_ "text-red-700 text-sm"] $ toHtml (show err :: String)
        NotConfigured -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 mb-1"] "⚠ Not configured"
            p_ [class_ "text-yellow-700 text-sm"] $ do
              "Config file not found at "
              code_ [class_ "bg-yellow-100 px-1 rounded"] "~/.config/attic/config.toml"
        NoServerForEndpoint endpoint -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 font-semibold mb-1"] "⚠ No server configured"
            p_ [class_ "text-yellow-700 text-sm"] $ do
              "No server found for endpoint: "
              code_ [class_ "bg-yellow-100 px-1 rounded"] $ toHtml (toText endpoint)
        NoToken serverName -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 font-semibold mb-1"] "⚠ Missing authentication token"
            p_ [class_ "text-yellow-700 text-sm"] $ do
              "Server "
              strong_ $ toHtml serverName
              " is configured but has no authentication token"
      Right atticCfg -> do
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
                  code_ [class_ "bg-green-100 px-1 rounded"] $ toHtml (toText serverCfg.endpoint)
                  span_ [class_ "ml-2 text-green-600"] "🔑"
