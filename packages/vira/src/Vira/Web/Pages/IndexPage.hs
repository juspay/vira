-- | Top-level routes and views
module Vira.Web.Pages.IndexPage where

import Lucid
import Servant.API (Get, NamedRoutes, QueryParam, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (fieldLink, linkURI)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type qualified as St
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Pages.CachePage qualified as CachePage
import Vira.Web.Pages.EnvironmentPage qualified as EnvironmentPage
import Vira.Web.Pages.EventsPage qualified as EventsPage
import Vira.Web.Pages.JobPage qualified as JobPage
import Vira.Web.Pages.RegistryPage qualified as RegistryPage
import Vira.Web.Servant ((//))
import Vira.Web.Stack qualified as Web
import Vira.Web.Stream.ScopedRefresh qualified as Refresh
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- QueryParam "filter" ActivityFilter :> Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _environment :: mode :- "env" Servant.API.:> NamedRoutes EnvironmentPage.Routes
  , _cache :: mode :- "cache" Servant.API.:> NamedRoutes CachePage.Routes
  , _events :: mode :- "events" Servant.API.:> NamedRoutes EventsPage.Routes
  , _refresh :: mode :- "refresh" Servant.API.:> Refresh.StreamRoute
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.GlobalSettings -> App.ViraRuntimeState -> App.WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _home =
        Web.runAppInServant globalSettings viraRuntimeState webSettings
          . runAppHtml
          . indexView
    , _repos = RegistryPage.handlers globalSettings viraRuntimeState webSettings
    , _jobs = JobPage.handlers globalSettings viraRuntimeState webSettings
    , _environment = EnvironmentPage.handlers globalSettings viraRuntimeState webSettings
    , _cache = CachePage.handlers globalSettings viraRuntimeState webSettings
    , _events = EventsPage.handlers globalSettings viraRuntimeState webSettings
    , _refresh =
        Web.runStreamHandler globalSettings viraRuntimeState . Refresh.streamRouteHandler
    }

activityLimit :: Natural
activityLimit = 15

data ActivityFilter = CIOnly | AllActivity
  deriving stock (Eq, Show)

instance FromHttpApiData ActivityFilter where
  parseQueryParam = \case
    "all" -> Right AllActivity
    _ -> Right CIOnly

instance ToHttpApiData ActivityFilter where
  toQueryParam = \case
    AllActivity -> "all"
    CIOnly -> "ci"

indexView :: Maybe ActivityFilter -> AppHtml ()
indexView mFilter = do
  logoUrl <- W.appLogoUrl
  allActivities <- lift $ App.query (St.GetAllBranchesA Nothing Nothing activityLimit)
  let filterMode = fromMaybe CIOnly mFilter
      -- By default, show only CI activity (branches with builds)
      ciActivities = filter (\d -> St.jobsCount d > 0) allActivities
      excludedCount = length allActivities - length ciActivities
      activities = case filterMode of
        CIOnly -> ciActivities
        AllActivity -> allActivities
      linkText = show . linkURI
      reposLink = linkText $ fieldLink _repos // RegistryPage._listing
      envLink = linkText $ fieldLink _environment // EnvironmentPage._view
      cacheLink = linkText $ fieldLink _cache // CachePage._view
  W.layout mempty $ do
    heroWelcome logoUrl reposLink envLink cacheLink
    unless (null activities) $
      viewRecentActivity filterMode excludedCount activities

viewRecentActivity :: ActivityFilter -> Int -> [St.BranchDetails] -> AppHtml ()
viewRecentActivity filterMode excludedCount activities = do
  W.viraSection_ [] $ do
    -- Header with title
    h2_ [class_ "text-2xl font-bold text-gray-900 dark:text-gray-100"] "Recent Activity"
    -- Tab bar
    div_ [class_ "border-b-2 border-gray-200 dark:border-gray-700"] $ do
      div_ [class_ "flex space-x-1 -mb-0.5"] $ do
        -- CI Activity tab (default)
        a_
          [ href_ "/"
          , class_ $
              "px-4 py-3 font-semibold text-sm transition-colors border-b-2 "
                <> case filterMode of
                  CIOnly -> "border-indigo-600 dark:border-indigo-400 text-indigo-600 dark:text-indigo-400 bg-indigo-50 dark:bg-indigo-900/20"
                  AllActivity -> "border-transparent text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-800/50"
          ]
          "CI Activity"
        -- All Activity tab
        when (excludedCount > 0) $ do
          a_
            [ href_ "/?filter=all"
            , class_ $
                "px-4 py-3 font-semibold text-sm transition-colors border-b-2 flex items-center gap-2 "
                  <> case filterMode of
                    AllActivity -> "border-indigo-600 dark:border-indigo-400 text-indigo-600 dark:text-indigo-400 bg-indigo-50 dark:bg-indigo-900/20"
                    CIOnly -> "border-transparent text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-800/50"
            ]
            $ do
              "All Activity"
              -- Badge with count
              span_
                [ class_ $
                    "inline-flex items-center justify-center px-2 py-0.5 text-xs font-semibold rounded-full "
                      <> case filterMode of
                        AllActivity -> "bg-indigo-600 dark:bg-indigo-500 text-white"
                        CIOnly -> "bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300"
                ]
                $ toHtml (show excludedCount :: String)
    -- Activity list
    div_ $ do
      forM_ activities $ \details ->
        W.viraBranchDetailsRow_ True details

heroWelcome :: (Monad m) => Text -> Text -> Text -> Text -> HtmlT m ()
heroWelcome logoUrl reposLink envLink cacheLink = do
  -- Compact hero banner
  div_ [class_ "bg-indigo-50 dark:bg-indigo-900/20 border-2 border-t-0 border-indigo-200 dark:border-indigo-800 rounded-b-xl p-6 mb-6"] $ do
    -- Logo and title
    div_ [class_ "flex items-center space-x-4 mb-4"] $ do
      img_ [src_ logoUrl, class_ "w-12 h-12"]
      div_ $ do
        h1_ [class_ "text-2xl font-bold text-indigo-900 dark:text-indigo-100"] $ do
          a_ [href_ "http://github.com/juspay/vira", class_ "hover:underline", target_ "_blank"] "Vira"
        p_ [class_ "text-sm text-indigo-700 dark:text-indigo-300"] $ do
          "No-frills CI/CD for "
          a_ [href_ "https://nixos.asia/en/nix-first", class_ "underline hover:no-underline", target_ "blank"] "Nix"

    -- Navigation links - prominent horizontal bar
    div_ [class_ "flex gap-3"] $ do
      heroButton reposLink Icon.book_2 "Repositories"
      heroButton envLink Icon.cpu "Environment"
      heroButton cacheLink Icon.database "Binary Cache"
  where
    heroButton :: (Monad m) => Text -> ByteString -> Text -> HtmlT m ()
    heroButton url icon label =
      a_ [href_ url, class_ "flex-1 flex items-center justify-center gap-2 py-2 px-4 bg-indigo-600 dark:bg-indigo-700 text-white font-semibold rounded-lg hover:bg-indigo-700 dark:hover:bg-indigo-600 transition-colors"] $ do
        div_ [class_ "w-4 h-4 flex items-center justify-center"] $ toHtmlRaw icon
        toHtml label
