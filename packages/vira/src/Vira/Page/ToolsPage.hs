-- | Tools page HTTP handlers
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
) where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum (..))
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Page.Common.User qualified as User
import Vira.Page.ToolsPage.View qualified as ToolView
import Vira.Tool.Core qualified as Tool
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
        ToolView.viewTool tool toolData
