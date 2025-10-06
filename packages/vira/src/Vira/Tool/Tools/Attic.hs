{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
  createPushProcess,
  viewToolStatus,
  ConfigError (..),
  AtticSuggestion (..),
  configErrorToSuggestion,
  suggestionToText,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), ConfigError (..))
import Attic.Config qualified
import Attic.Types (AtticCache (..), AtticServer (AtticServer, name), AtticServerEndpoint (..), AtticToken (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Process (CreateProcess)
import Lucid (HtmlT, ToHtml (..), class_, code_, div_, p_, span_, strong_, toHtml)
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

{- | Create attic push process from parsed cache info and config

Takes the attic config, server endpoint, cache name, and path to push.
Returns either a ConfigError or the CreateProcess for pushing.
-}
createPushProcess ::
  AtticConfig ->
  AtticServerEndpoint ->
  AtticCache ->
  FilePath ->
  Either ConfigError CreateProcess
createPushProcess config serverEndpoint cacheName path = do
  -- Get server name for endpoint
  serverName <-
    Attic.Config.lookupEndpoint config serverEndpoint
      & maybeToRight (NoServerForEndpoint serverEndpoint)

  -- Create the push process (token validation already done in getToolData)
  pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) cacheName path

-- | Suggestions for fixing Attic configuration issues
data AtticSuggestion = AtticLoginSuggestion
  { bin :: FilePath
  , serverName :: Text
  , endpoint :: AtticServerEndpoint
  , token :: Maybe AtticToken
  }
  deriving stock (Show, Eq)

-- | Convert a ConfigError to a suggestion for fixing it
configErrorToSuggestion :: Maybe AtticServerEndpoint -> ConfigError -> Maybe AtticSuggestion
configErrorToSuggestion mEndpoint = \case
  ParseError _ -> Nothing -- Parse errors need manual TOML fixing
  NotConfigured -> Just v
  NoServerForEndpoint ep -> Just $ v {endpoint = ep, serverName = deriveServerName ep}
  NoToken server -> Just $ v {serverName = server.name}
  where
    v =
      AtticLoginSuggestion
        { bin = Attic.atticBin
        , serverName = deriveServerName $ fromMaybe "https://cache.example.com" mEndpoint
        , endpoint = fromMaybe "https://cache.example.com" mEndpoint
        , token = Nothing
        }
    deriveServerName (AtticServerEndpoint url) =
      T.replace "https://" "" url
        & T.replace "http://" ""
        & T.replace "." "-"
        & T.replace "/" ""

-- | Convert suggestion to text for CI logs
suggestionToText :: AtticSuggestion -> Text
suggestionToText suggestion =
  T.intercalate
    "\n"
    [ "ATTIC=" <> toText suggestion.bin
    , "TOKEN=" <> tokenText
    , "$ATTIC login " <> suggestion.serverName <> " " <> toText suggestion.endpoint <> " $TOKEN"
    ]
  where
    tokenText = maybe "<token>" (toText . unAtticToken) suggestion.token

-- | ToHtml instance for rendering suggestions in the Tools Page
instance ToHtml AtticSuggestion where
  toHtmlRaw = toHtml
  toHtml suggestion = do
    div_ [class_ "mt-2"] $ do
      p_ [class_ "text-sm text-yellow-700 dark:text-yellow-300 mb-1"] "Run:"
      W.viraCodeCopyable_ $ suggestionToText suggestion

-- | View Attic tool status
viewToolStatus :: (Monad m) => Either ConfigError AtticConfig -> HtmlT m ()
viewToolStatus result = do
  div_ [class_ "mb-3"] $ do
    case result of
      Left setupErr -> case setupErr of
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
            forM_ (configErrorToSuggestion Nothing NotConfigured) toHtml
        NoServerForEndpoint endpoint -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 dark:text-yellow-200 font-semibold mb-1"] "No server configured"
            p_ [class_ "text-yellow-700 dark:text-yellow-300 text-sm"] $ do
              "No server found for endpoint: "
              code_ [class_ "bg-yellow-100 dark:bg-yellow-800 px-1 rounded"] $ toHtml (toText endpoint)
            forM_ (configErrorToSuggestion Nothing (NoServerForEndpoint endpoint)) toHtml
        NoToken serverName -> do
          viraAlert_ AlertWarning $ do
            p_ [class_ "text-yellow-800 dark:text-yellow-200 font-semibold mb-1"] "Missing authentication token"
            p_ [class_ "text-yellow-700 dark:text-yellow-300 text-sm"] $ do
              "Server "
              strong_ $ toHtml serverName.name
              " is configured but has no authentication token"
            forM_ (configErrorToSuggestion Nothing (NoToken serverName)) toHtml
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
