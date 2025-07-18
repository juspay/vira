module Vira.Page.SettingsPage (
  Routes (..),
  handlers,
)
where

import Lucid
import Servant.API (Get)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Widgets qualified as W
import Prelude hiding (Reader, ask, runReader)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _view = App.runAppInServant cfg $ do
        let crumbs = [LinkTo.Settings]
        pure $ W.layout cfg crumbs $ do
          pass
    }
