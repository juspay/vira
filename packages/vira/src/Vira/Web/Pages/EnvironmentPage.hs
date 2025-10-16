-- | Environment page HTTP handlers and views
module Vira.Web.Pages.EnvironmentPage (
  Routes (..),
  handlers,
) where

import Servant
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.Pages.EnvironmentPage.Builders qualified as Builders
import Vira.Web.Pages.EnvironmentPage.Tools qualified as Tools

data Routes mode = Routes
  { _tools :: mode :- "tools" :> NamedRoutes Tools.Routes
  , _builders :: mode :- "builders" :> NamedRoutes Builders.Routes
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _tools = Tools.handlers globalSettings viraRuntimeState webSettings
    , _builders = Builders.handlers globalSettings viraRuntimeState webSettings
    }
