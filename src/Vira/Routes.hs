-- | Top-level routes and views
module Vira.Routes where

import Lucid
import Servant.API (Get, NamedRoutes, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (fieldLink, linkURI)
import Servant.Server.Generic (AsServer)
import Vira.App ((//))
import Vira.App qualified as App
import Vira.App.LinkTo.Type (LinkTo (..))
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Status qualified as Status
import Vira.Widgets qualified as W
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _about :: mode :- "about" Servant.API.:> Get '[HTML] (Html ())
  , _status :: mode :- "status" Servant.API.:> NamedRoutes Status.Routes
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _home = App.runAppInServant cfg $ do
        pure $ W.layout cfg "Welcome" [] $ do
          nav_ [class_ "space-y-2"] $ do
            forM_ menu $ \(name, url) -> do
              a_ [href_ url, class_ "flex items-center p-3 space-x-3 text-blue-700 font-bold transition-colors rounded-md hover:bg-gray-100"] $ do
                name
    , _repos = RegistryPage.handlers cfg
    , _jobs = JobPage.handlers cfg
    , _about = do
        pure $ W.layout cfg "About Vira" [About] $ do
          div_ $ do
            a_ [href_ "https://github.com/juspay/vira"] "GitHub Repo"
    , _status = Status.handlers cfg
    }
  where
    linkText = show . linkURI
    menu :: [(Html (), Text)]
    menu =
      [ ("Repos", linkText $ fieldLink _repos // RegistryPage._listing)
      , ("About", linkText $ fieldLink _about)
      ]
