{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Environment.Tool.Tools.Attic (
  getToolData,
  viewToolStatus,
  ConfigError (..),
  AtticSuggestion (..),
  configErrorToSuggestion,
  getAtticConfig,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Attic.Types (AtticServer (..), AtticServerEndpoint (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Lucid (HtmlT, ToHtml (..), class_, code_, div_, p_, span_, strong_, toHtml)
import TOML (TOMLError)
import Text.Show qualified as TS
import Vira.Environment.Tool.Type.ToolData (ToolData (..))
import Vira.Web.Widgets.Alert (AlertType (..), viraAlertWithTitle_, viraAlert_)
import Vira.Web.Widgets.Code qualified as W

-- | Configuration errors
data ConfigError
  = -- | TOML configuration parse error
    ParseError TOMLError
  | -- | No server configured for endpoint
    MissingEndpoint AtticServerEndpoint
  | -- | Server configured but no authentication token
    MissingToken AtticServer
  deriving stock (Show, Eq)

{- | Get validated Attic configuration

Combines reading and validation into a single operation.
Returns:
- Left ConfigError if config is invalid or incomplete
- Right AtticConfig if successfully parsed and validated (empty config if file doesn't exist)
-}
getAtticConfig :: IO (Either ConfigError AtticConfig)
getAtticConfig = validateConfig <$> Attic.Config.readAtticConfig
  where
    validateConfig :: Either TOMLError (Maybe AtticConfig) -> Either ConfigError AtticConfig
    validateConfig result = do
      mConfig <- first ParseError result
      let config = fromMaybe emptyConfig mConfig

      -- Check if any server is missing a token
      case find (\(_server, serverCfg) -> isNothing serverCfg.token) (Map.toList config.servers) of
        Just (serverName, cfg) -> Left $ MissingToken $ AtticServer serverName cfg.endpoint
        Nothing -> Right config

    emptyConfig = AtticConfig {defaultServer = Nothing, servers = Map.empty}

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either ConfigError AtticConfig))
getToolData = do
  status <- liftIO getAtticConfig
  pure
    ToolData
      { name = "Attic"
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
configErrorToSuggestion :: ConfigError -> AtticSuggestion
configErrorToSuggestion = \case
  ParseError _ ->
    AtticParseErrorSuggestion "~/.config/attic/config.toml"
  MissingEndpoint ep ->
    mkLoginSuggestion (deriveServerName ep) ep
  MissingToken server ->
    mkLoginSuggestion server.name server.endpoint
  where
    mkLoginSuggestion :: Text -> AtticServerEndpoint -> AtticSuggestion
    mkLoginSuggestion name endpoint =
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , endpoint = endpoint
        , serverName = name
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
      W.viraCodeBlockCopyable Nothing $ show @Text suggestion

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either ConfigError AtticConfig -> HtmlT m ()
viewToolStatus result = do
  div_ [class_ "mb-3"] $ do
    case result of
      Left setupErr -> do
        let suggestion = configErrorToSuggestion setupErr
        case setupErr of
          ParseError err ->
            viraAlertWithTitle_ AlertError "Parse error" $
              toHtml (show err :: String) >> toHtml suggestion
          MissingEndpoint endpoint -> viraAlertWithTitle_ AlertWarning "No server configured" $ do
            "No server found for endpoint: "
            code_ [class_ "bg-yellow-100 dark:bg-yellow-800 px-1 rounded"] $ toHtml (toText endpoint)
            toHtml suggestion
          MissingToken serverName -> viraAlertWithTitle_ AlertWarning "Missing authentication token" $ do
            "Server "
            strong_ $ toHtml serverName.name
            " is configured but has no authentication token"
            toHtml suggestion
      Right atticCfg ->
        if Map.null atticCfg.servers
          then
            let suggestion =
                  AtticLoginSuggestion
                    { bin = Attic.atticBin
                    , serverName = "cache-example-com"
                    , endpoint = "https://cache.example.com"
                    }
             in viraAlertWithTitle_ AlertInfo "Not configured" $ do
                  "No Attic servers configured. "
                  toHtml suggestion
          else viraAlert_ AlertSuccess $ do
            case atticCfg.defaultServer of
              Just defServer -> do
                p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] $ do
                  "Default server: "
                  strong_ $ toHtml defServer
              Nothing -> do
                p_ [class_ "text-green-800 dark:text-green-200 font-semibold mb-1"] "Configured"

            -- Display all configured servers
            p_ [class_ "text-green-700 text-xs mt-2 mb-1"] "Configured servers:"
            div_ [class_ "space-y-1"] $ do
              forM_ (Map.toList atticCfg.servers) $ \(serverName, serverCfg) -> do
                div_ [class_ "text-green-700 text-xs pl-2"] $ do
                  strong_ $ toHtml serverName
                  ": "
                  code_ [class_ "bg-green-100 px-1 rounded"] $ toHtml (toText serverCfg.endpoint)
                  span_ [class_ "ml-2 text-green-600"] "🔑"
