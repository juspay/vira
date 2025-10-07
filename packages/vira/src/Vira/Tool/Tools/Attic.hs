{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  viewToolStatus,
  ConfigError (..),
  AtticSuggestion (..),
  configErrorToSuggestion,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), ConfigError (..))
import Attic.Config qualified
import Attic.Types (AtticServer (..), AtticServerEndpoint (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Lucid (HtmlT, ToHtml (..), class_, code_, div_, p_, span_, strong_, toHtml)
import Text.Show qualified as TS
import Vira.Tool.Type.ToolData (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)
import Vira.Widgets.Code qualified as W

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

-- | Suggestions for fixing Attic configuration issues
data AtticSuggestion
  = AtticLoginSuggestion
      { bin :: FilePath
      , serverName :: Text
      , endpoint :: AtticServerEndpoint
      }
  | AtticParseErrorSuggestion
      { configPath :: Text
      }
  deriving stock (Eq)

instance TS.Show AtticSuggestion where
  show = \case
    AtticLoginSuggestion {bin, serverName, endpoint} ->
      toString $
        T.intercalate
          "\n"
          [ "ATTIC=" <> toText bin
          , "TOKEN=YOUR-TOKEN-HERE"
          , "$ATTIC login " <> serverName <> " " <> toText endpoint <> " $TOKEN"
          ]
    AtticParseErrorSuggestion {configPath} ->
      toString $ "rm " <> configPath

-- | Convert a ConfigError to a suggestion for fixing it
configErrorToSuggestion :: Maybe AtticServerEndpoint -> ConfigError -> AtticSuggestion
configErrorToSuggestion mEndpoint = \case
  ParseError _ ->
    AtticParseErrorSuggestion "~/.config/attic/config.toml"
  NotConfigured ->
    mkLoginSuggestion $ fromMaybe "https://cache.example.com" mEndpoint
  NoServerForEndpoint ep ->
    mkLoginSuggestion ep
  NoToken server ->
    mkLoginSuggestion server.endpoint
  where
    mkLoginSuggestion :: AtticServerEndpoint -> AtticSuggestion
    mkLoginSuggestion endpoint =
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , endpoint = endpoint
        , serverName = deriveServerName endpoint
        }
    deriveServerName (AtticServerEndpoint url) =
      T.replace "https://" "" url
        & T.replace "http://" ""
        & T.replace "." "-"
        & T.replace "/" ""

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml AtticSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-yellow-700 dark:text-yellow-300 mb-1"] "Run:"
      W.viraCodeCopyable_ $ show @Text suggestion

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either ConfigError AtticConfig -> HtmlT m ()
viewToolStatus result = do
  div_ [class_ "mb-3"] $ do
    case result of
      Left setupErr -> do
        let suggestion = configErrorToSuggestion Nothing setupErr
        case setupErr of
          ParseError err -> do
            viraAlert_ AlertError $ do
              p_ [class_ "text-red-800 dark:text-red-200 font-semibold mb-1"] "Parse error"
              p_ [class_ "text-red-700 dark:text-red-300 text-sm"] $ toHtml (show err :: String)
          NotConfigured -> do
            viraAlert_ AlertWarning $ do
              p_ [class_ "text-yellow-800 dark:text-yellow-200 mb-1"] "Not configured"
              p_ [class_ "text-yellow-700 dark:text-yellow-300 text-sm"] $ do
                "Config file not found at "
                code_ [class_ "bg-yellow-100 dark:bg-yellow-800 px-1 rounded"] "~/.config/attic/config.toml"
              toHtml suggestion
          NoServerForEndpoint endpoint -> do
            viraAlert_ AlertWarning $ do
              p_ [class_ "text-yellow-800 dark:text-yellow-200 font-semibold mb-1"] "No server configured"
              p_ [class_ "text-yellow-700 dark:text-yellow-300 text-sm"] $ do
                "No server found for endpoint: "
                code_ [class_ "bg-yellow-100 dark:bg-yellow-800 px-1 rounded"] $ toHtml (toText endpoint)
              toHtml suggestion
          NoToken serverName -> do
            viraAlert_ AlertWarning $ do
              p_ [class_ "text-yellow-800 dark:text-yellow-200 font-semibold mb-1"] "Missing authentication token"
              p_ [class_ "text-yellow-700 dark:text-yellow-300 text-sm"] $ do
                "Server "
                strong_ $ toHtml serverName.name
                " is configured but has no authentication token"
              toHtml suggestion
      Right atticCfg -> do
        viraAlert_ AlertSuccess $ do
          case atticCfg.defaultServer of
            Just defServer -> do
              p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] $ do
                "Default server: "
                strong_ $ toHtml defServer
            Nothing -> do
              p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] "Configured"

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
