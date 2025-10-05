{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools page HTTP handlers and views
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
) where

import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GH.Auth.Status (AuthStatus (..))
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Page.Common.User qualified as User
import Vira.Tool.Core (Tool (..), ToolData (..))
import Vira.Tool.Core qualified as Tool
import Vira.Widgets.Alert qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.AppState -> WebSettings -> Routes AsServer
handlers cfg webSettings =
  Routes
    { _view = App.runAppInServant cfg webSettings . App.runAppHtml $ viewHandler
    }

viewHandler :: AppHtml ()
viewHandler = W.layout [LinkTo.Tools] viewTools

viewTools :: AppHtml ()
viewTools = do
  -- Refresh tools data every time the page is loaded
  toolsMap <- lift Tool.refreshTools
  let tools = DMap.toList toolsMap

  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.tool) "Tools" $ do
      div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300"] "Command-line tools used by Vira jobs"
        span_ [class_ "text-indigo-800 dark:text-indigo-300 font-semibold"] User.viewUserInfo

    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      forM_ tools $ \(tool :=> toolData) ->
        viewTool tool toolData

-- | View a tool card with its metadata and runtime info
viewTool :: (Monad m) => Tool info -> ToolData info -> HtmlT m ()
viewTool tool toolData = do
  let disp = viewToolDisplay tool
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      span_ [class_ $ "h-12 w-12 mr-4 " <> disp.bgClass <> " rounded-lg flex items-center justify-center " <> disp.textClass <> " font-bold text-xl"] $
        toHtml disp.initial
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 dark:text-gray-100 mb-2"] $ toHtml toolData.name
        p_ [class_ "text-gray-600 dark:text-gray-300 text-sm mb-3"] $ toHtml toolData.description
        div_ [class_ "mb-3 space-y-1"] $ do
          forM_ toolData.binPaths $ \binPath ->
            code_ [class_ "block text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 px-2 py-1 rounded font-mono"] $ toHtml binPath

        -- Render tool-specific info
        viewToolInfo tool toolData.info

        a_
          [ href_ toolData.url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 dark:text-indigo-400 hover:text-indigo-800 dark:hover:text-indigo-300 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link

-- | View tool-specific runtime information
viewToolInfo :: (Monad m) => Tool info -> info -> HtmlT m ()
viewToolInfo Attic cfg = do
  div_ [class_ "mb-3"] $ do
    case cfg of
      Left err -> do
        W.viraAlert_ W.AlertError $ do
          p_ [class_ "text-red-800 font-semibold mb-1"] "✗ Parse error"
          p_ [class_ "text-red-700 text-sm"] $ toHtml (show err :: String)
      Right Nothing -> do
        W.viraAlert_ W.AlertWarning $ do
          p_ [class_ "text-yellow-800 mb-1"] "⚠ Not configured"
          p_ [class_ "text-yellow-700 text-sm"] $ do
            "Config file not found at "
            code_ [class_ "bg-yellow-100 px-1 rounded"] "~/.config/attic/config.toml"
      Right (Just atticCfg) -> do
        W.viraAlert_ W.AlertSuccess $ do
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
viewToolInfo GitHub status = do
  div_ [class_ "mb-3"] $ do
    case status of
      Authenticated {host, login, scopes} -> do
        W.viraAlert_ W.AlertSuccess $ do
          p_ [class_ "text-green-800 font-semibold mb-1"] $ do
            "✓ Authenticated as "
            strong_ $ toHtml login
            " on "
            strong_ $ toHtml host
          p_ [class_ "text-green-700 text-xs"] $ do
            "Scopes: "
            toHtml $ T.intercalate ", " scopes
      NotAuthenticated -> do
        W.viraAlert_ W.AlertError $ do
          p_ [class_ "text-red-800 mb-1"] "✗ Not authenticated"
          p_ [class_ "text-red-700 text-sm"] $ do
            "Run "
            code_ [class_ "bg-red-100 px-1 rounded"] "gh auth login"
            " to authenticate."
viewToolInfo Omnix () = mempty
viewToolInfo Git () = mempty
viewToolInfo Cachix () = mempty

-- | Tool display styling
data ToolDisplay = ToolDisplay
  { initial :: Text
  , bgClass :: Text
  , textClass :: Text
  }

-- | Get display styling for a tool
viewToolDisplay :: Tool info -> ToolDisplay
viewToolDisplay = \case
  Attic -> ToolDisplay {initial = "A", bgClass = "bg-indigo-100", textClass = "text-indigo-600"}
  GitHub -> ToolDisplay {initial = "G", bgClass = "bg-green-100", textClass = "text-green-600"}
  Omnix -> ToolDisplay {initial = "O", bgClass = "bg-purple-100", textClass = "text-purple-600"}
  Git -> ToolDisplay {initial = "G", bgClass = "bg-orange-100", textClass = "text-orange-600"}
  Cachix -> ToolDisplay {initial = "C", bgClass = "bg-blue-100", textClass = "text-blue-600"}
