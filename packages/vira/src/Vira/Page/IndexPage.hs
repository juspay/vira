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
import Vira.App.Lucid (runAppHtml)
import Vira.App.Servant (mapSourceT)
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.ToolsPage qualified as ToolsPage
import Vira.Stream.Refresh qualified as Refresh
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Layout qualified as W
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _tools :: mode :- "tools" Servant.API.:> NamedRoutes ToolsPage.Routes
  , _refresh :: mode :- "refresh" Servant.API.:> Refresh.StreamRoute
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.GlobalSettings -> App.AppState -> App.WebSettings -> Routes AsServer
handlers globalSettings appState webSettings =
  Routes
    { _home =
        App.runAppInServant globalSettings appState webSettings $
          runAppHtml $ do
            logoUrl <- W.appLogoUrl
            W.layout mempty $
              heroWelcome logoUrl menu
    , _repos = RegistryPage.handlers globalSettings appState webSettings
    , _jobs = JobPage.handlers globalSettings appState webSettings
    , _tools = ToolsPage.handlers globalSettings appState webSettings
    , _refresh =
        pure $ recommendedEventSourceHeaders $ mapSourceT (App.runApp globalSettings appState) Refresh.streamRouteHandler
    }
  where
    linkText = show . linkURI
    menu :: (Monad m) => [(HtmlT m (), Text)]
    menu =
      [ ("Repositories", linkText $ fieldLink _repos // RegistryPage._listing)
      , ("Tools", linkText $ fieldLink _tools // ToolsPage._view)
      ]

heroWelcome :: (Monad m) => Text -> [(HtmlT m (), Text)] -> HtmlT m ()
heroWelcome logoUrl menu = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-12 mb-8 text-center"] $ do
    h1_ [class_ "text-5xl font-bold text-indigo-900 tracking-tight mb-4"] $ do
      "Welcome to "
      a_ [href_ "http://github.com/juspay/vira", class_ "underline hover:no-underline", target_ "_blank"] "Vira"
    img_ [src_ logoUrl, class_ "mx-auto w-48 h-48 mb-4"]
    p_ [class_ "text-xl text-indigo-700 mb-8"] $ do
      "No-frills CI/CD for teams using "
      a_ [href_ "https://nixos.asia/en/nix-first", class_ "underline hover:no-underline", target_ "blank"] "Nix"

  -- Navigation cards
  div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-3"] $ do
    forM_ menu $ \(name, url) -> do
      W.viraNavigationCard_ url name
