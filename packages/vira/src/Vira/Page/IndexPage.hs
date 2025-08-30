-- | Top-level routes and views
module Vira.Page.IndexPage where

import Lucid
import Servant.API (Get, NamedRoutes, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.EventStream (recommendedEventSourceHeaders)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (fieldLink, linkURI)
import Servant.Server.Generic (AsServer)
import Vira.App ((//))
import Vira.App qualified as App
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.SettingsPage qualified as SettingsPage
import Vira.Stream.Status qualified as Status
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Layout qualified as W
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _settings :: mode :- "settings" Servant.API.:> NamedRoutes SettingsPage.Routes
  , _status :: mode :- "status" Servant.API.:> Status.StreamRoute
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _home = App.runAppInServant cfg $ do
        App.runVHtmlInServant $
          W.layout cfg [] $
            heroWelcome menu
    , _repos = RegistryPage.handlers cfg
    , _jobs = JobPage.handlers cfg
    , _settings = SettingsPage.handlers cfg
    , _status = pure $ recommendedEventSourceHeaders $ Status.streamRouteHandler cfg
    }
  where
    linkText = show . linkURI
    menu :: (Monad m) => [(HtmlT m (), Text)]
    menu =
      [ ("Repositories", linkText $ fieldLink _repos // RegistryPage._listing)
      , ("Settings", linkText $ fieldLink _settings // SettingsPage._view)
      ]

heroWelcome :: (Monad m) => [(HtmlT m (), Text)] -> HtmlT m ()
heroWelcome menu = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-12 mb-8 text-center"] $ do
    h1_ [class_ "text-5xl font-bold text-indigo-900 tracking-tight mb-4"] $ do
      "Welcome to "
      a_ [href_ "http://github.com/juspay/vira", class_ "underline hover:no-underline", target_ "_blank"] "Vira"
    p_ [class_ "text-xl text-indigo-700 mb-8"] $ do
      "No-frills CI/CD for teams using "
      a_ [href_ "https://nixos.asia/en/nix-first", class_ "underline hover:no-underline", target_ "blank"] "Nix"

  -- Navigation cards
  div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-3"] $ do
    forM_ menu $ \(name, url) -> do
      W.viraNavigationCard_ url name
