{-# LANGUAGE OverloadedRecordDot #-}

-- | Environment page HTTP handlers and views
module Vira.Web.Pages.EnvironmentPage (
  Routes (..),
  handlers,
) where

import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Environment.Tool.Core (Tools (..))
import Vira.Environment.Tool.Type.ToolData (ToolData (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Pages.Common.User qualified as User
import Vira.Web.Pages.EnvironmentPage.Builders qualified as Builders
import Vira.Web.Pages.EnvironmentPage.Cache qualified as Cache
import Vira.Web.Pages.EnvironmentPage.Tools qualified as Tools
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler globalSettings
    }

viewHandler :: App.GlobalSettings -> AppHtml ()
viewHandler globalSettings = W.layout [LinkTo.Environment] (viewEnvironment globalSettings)

viewEnvironment :: App.GlobalSettings -> AppHtml ()
viewEnvironment globalSettings = do
  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.cpu) "Environment" $ do
      div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300"] "User environment under which Vira runs"
        span_ [class_ "text-indigo-800 dark:text-indigo-300 font-semibold"] User.viewUserInfo

    -- Cache Section
    Cache.viewCache globalSettings

    -- Tools Section (returns tools data for reuse)
    tools <- Tools.viewTools

    -- Builders Section (reuses nix config from tools)
    Builders.viewBuilders tools.nix.status
