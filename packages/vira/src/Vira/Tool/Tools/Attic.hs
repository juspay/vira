{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
  viewToolStatus,
  ConfigError (..),
  AtticError (..),
  AtticSuggestion (..),
  configErrorToSuggestion,
  suggestionToText,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..), ConfigError (..))
import Attic.Config qualified
import Attic.Types (AtticServer (..), AtticServerEndpoint (..))
import Attic.Url qualified as Url
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import Lucid (HtmlT, ToHtml (..), class_, code_, div_, p_, span_, strong_, toHtml)
import Vira.Tool.Type.ToolData (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)
import Vira.Widgets.Code qualified as W

-- | All errors that can occur when working with Attic
data AtticError
  = -- | URL parsing failed
    UrlParseError Url.ParseError
  | -- | Configuration/setup error
    ConfigError ConfigError
  deriving stock (Show, Eq)

-- | Suggestions for fixing Attic configuration issues
data AtticSuggestion = AtticLoginSuggestion
  { bin :: FilePath
  , serverName :: Text
  , endpoint :: AtticServerEndpoint
  , tokenPlaceholder :: Text
  }
  deriving stock (Show, Eq)

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either ConfigError AtticConfig))
getToolData = do
  status <- liftIO Attic.Config.getAtticConfig
  pure
    ToolData
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = one $ toText Attic.atticBin
      , status = status
      }

{- | Create attic push process from cache URL and config

Takes the attic config result, cache URL, and path to push.
Returns either an AtticError or the CreateProcess for pushing.
-}
createPushProcess ::
  Either ConfigError AtticConfig ->
  Text ->
  FilePath ->
  Either AtticError CreateProcess
createPushProcess configResult cacheUrl path = do
  -- Parse cache URL to extract server endpoint and cache name
  (serverEndpoint, cacheName) <- first UrlParseError $ Url.parseCacheUrl cacheUrl

  -- Extract config
  config <- first ConfigError configResult

  -- Find server config that matches the endpoint
  (serverName, _serverCfg) <-
    find (\(_name, serverCfg) -> serverCfg.endpoint == serverEndpoint) (Map.toList config.servers)
      & maybeToRight (ConfigError (NoServerForEndpoint serverEndpoint))

  -- Create the push process (token validation already done in getToolData)
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) cacheName path

-- | Convert a ConfigError to a suggestion for fixing it
configErrorToSuggestion :: ConfigError -> Maybe AtticSuggestion
configErrorToSuggestion = \case
  ParseError _ -> Nothing -- Parse errors need manual TOML fixing
  NotConfigured ->
    Just $
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , serverName = "myserver"
        , endpoint = "https://cache.example.com"
        , tokenPlaceholder = "<token>"
        }
  NoServerForEndpoint endpoint ->
    Just $
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , serverName = "myserver"
        , endpoint = endpoint
        , tokenPlaceholder = "<token>"
        }
  NoToken serverName ->
    Just $
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , serverName = serverName
        , endpoint = "https://cache.example.com"
        , tokenPlaceholder = "<token>"
        }

-- | Convert suggestion to text for CI logs
suggestionToText :: AtticSuggestion -> Text
suggestionToText AtticLoginSuggestion {bin, serverName, endpoint, tokenPlaceholder} =
  toText bin <> " login " <> serverName <> " " <> toText endpoint <> " " <> tokenPlaceholder

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml AtticSuggestion where
  toHtmlRaw = toHtml
  toHtml AtticLoginSuggestion {bin, serverName, endpoint, tokenPlaceholder} = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-yellow-700 dark:text-yellow-300 mb-1"] "Run:"
      W.viraCodeCopyable_ $ toText bin <> " login " <> serverName <> " " <> toText endpoint <> " " <> tokenPlaceholder

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either ConfigError AtticConfig -> HtmlT m ()
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
            forM_ (configErrorToSuggestion NotConfigured) toHtml
        NoServerForEndpoint endpoint -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 font-semibold mb-1"] "⚠ No server configured"
            p_ [class_ "text-yellow-700 text-sm"] $ do
              "No server found for endpoint: "
              code_ [class_ "bg-yellow-100 px-1 rounded"] $ toHtml (toText endpoint)
            forM_ (configErrorToSuggestion (NoServerForEndpoint endpoint)) toHtml
        NoToken serverName -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 font-semibold mb-1"] "⚠ Missing authentication token"
            p_ [class_ "text-yellow-700 text-sm"] $ do
              "Server "
              strong_ $ toHtml serverName
              " is configured but has no authentication token"
            forM_ (configErrorToSuggestion (NoToken serverName)) toHtml
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
