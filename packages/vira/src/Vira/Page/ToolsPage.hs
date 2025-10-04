{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools information page
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
)
where

import Data.Dependent.Sum (DSum (..))
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Page.ToolsPage.Tool qualified as Tool
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
  -- Read all tool infos (preserves order)
  toolInfos <- lift $ liftIO Tool.readAllTools

  W.viraSection_ [] $ do
    W.viraPageHeader_ "Tools" $ do
      p_ [class_ "text-gray-600"] "Command-line tools used by Vira jobs"
    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      forM_ toolInfos $ \(tool :=> Identity info) ->
        toolCard tool info

toolCard :: (Monad m) => Tool.Tool info -> info -> HtmlT m ()
toolCard tool info = do
  let meta = Tool.toolMeta tool
      disp = Tool.toolDisplay tool
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      span_ [class_ $ "h-12 w-12 mr-4 " <> disp.bgClass <> " rounded-lg flex items-center justify-center " <> disp.textClass <> " font-bold text-xl"] $
        toHtml disp.initial
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 mb-2"] $ toHtml meta.name
        p_ [class_ "text-gray-600 text-sm mb-3"] $ toHtml meta.description
        div_ [class_ "mb-3 space-y-1"] $ do
          forM_ meta.binPaths $ \binPath ->
            code_ [class_ "block text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded font-mono"] $ toHtml binPath

        -- Render tool-specific info
        Tool.renderInfo tool info

        a_
          [ href_ meta.url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 hover:text-indigo-800 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link
