{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools page HTTP handlers and views
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
) where

import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Page.Common.User qualified as User
import Vira.Tool.Core (ToolData (..), Tools (..))
import Vira.Tool.Core qualified as Tool
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Tools.GitHub qualified as GitHubTool
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
  tools <- lift Tool.refreshTools

  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.tool) "Tools" $ do
      div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300"] "Command-line tools used by Vira jobs"
        span_ [class_ "text-indigo-800 dark:text-indigo-300 font-semibold"] User.viewUserInfo

    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      viewToolCard "Attic" tools.attic (AtticTool.viewToolStatus tools.attic.status)
      viewToolCard "GitHub" tools.github (GitHubTool.viewToolStatus tools.github.status)
      viewToolCard "Omnix" tools.omnix mempty
      viewToolCard "Git" tools.git mempty
      viewToolCard "Cachix" tools.cachix mempty

-- | View a tool card with its metadata and runtime info
viewToolCard :: (Monad m) => Text -> ToolData statusType -> HtmlT m () -> HtmlT m ()
viewToolCard toolName toolData infoHtml = do
  let disp = viewToolDisplay toolName
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
        infoHtml

        a_
          [ href_ toolData.url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 dark:text-indigo-400 hover:text-indigo-800 dark:hover:text-indigo-300 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link

-- | Tool display styling
data ToolDisplay = ToolDisplay
  { initial :: Text
  , bgClass :: Text
  , textClass :: Text
  }

-- | Get display styling for a tool
viewToolDisplay :: Text -> ToolDisplay
viewToolDisplay = \case
  "Attic" -> ToolDisplay {initial = "A", bgClass = "bg-indigo-100", textClass = "text-indigo-600"}
  "GitHub" -> ToolDisplay {initial = "G", bgClass = "bg-green-100", textClass = "text-green-600"}
  "Omnix" -> ToolDisplay {initial = "O", bgClass = "bg-purple-100", textClass = "text-purple-600"}
  "Git" -> ToolDisplay {initial = "G", bgClass = "bg-orange-100", textClass = "text-orange-600"}
  "Cachix" -> ToolDisplay {initial = "C", bgClass = "bg-blue-100", textClass = "text-blue-600"}
  _ -> ToolDisplay {initial = "?", bgClass = "bg-gray-100", textClass = "text-gray-600"}
