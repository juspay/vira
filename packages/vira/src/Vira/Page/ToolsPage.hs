{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools information page
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
)
where

import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Page.ToolsPage.Attic qualified as Attic
import Vira.Page.ToolsPage.Cachix qualified as Cachix
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))
import Vira.Page.ToolsPage.Git qualified as Git
import Vira.Page.ToolsPage.GitHub qualified as GitHub
import Vira.Page.ToolsPage.Omnix qualified as Omnix
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
  ghInfo <- lift $ liftIO GitHub.read
  atticInfo <- lift $ liftIO Attic.read

  W.viraSection_ [] $ do
    W.viraPageHeader_ "Tools" $ do
      p_ [class_ "text-gray-600"] "Command-line tools used by Vira jobs"

    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      toolCard Omnix.display Omnix.tool mempty
      toolCard Git.display Git.tool mempty
      toolCard Attic.display Attic.tool (Attic.view atticInfo)
      toolCard Cachix.display Cachix.tool mempty
      toolCard GitHub.display GitHub.tool (GitHub.view ghInfo)

toolCard :: (Monad m) => ToolDisplay -> Tool -> HtmlT m () -> HtmlT m ()
toolCard disp tool extraInfo = do
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      span_ [class_ $ "h-12 w-12 mr-4 " <> disp.bgClass <> " rounded-lg flex items-center justify-center " <> disp.textClass <> " font-bold text-xl"] $
        toHtml disp.initial
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 mb-2"] $ toHtml tool.name
        p_ [class_ "text-gray-600 text-sm mb-3"] $ toHtml tool.description
        div_ [class_ "mb-3 space-y-1"] $ do
          forM_ tool.binPaths $ \binPath ->
            code_ [class_ "block text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded font-mono"] $ toHtml binPath

        -- Extra info (e.g., auth status)
        extraInfo

        a_
          [ href_ tool.url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 hover:text-indigo-800 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link
