{-# LANGUAGE OverloadedRecordDot #-}

-- | Top-level routes and views
module Vira.Web.Pages.IndexPage where

import Lucid
import Servant.API (Get, NamedRoutes, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (fieldLink, linkURI)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl, runAppHtml)
import Vira.Web.Pages.EnvironmentPage qualified as EnvironmentPage
import Vira.Web.Pages.JobPage qualified as JobPage
import Vira.Web.Pages.RegistryPage qualified as RegistryPage
import Vira.Web.Servant ((//))
import Vira.Web.Stack qualified as Web
import Vira.Web.Stream.ScopedRefresh qualified as Refresh
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _environment :: mode :- "env" Servant.API.:> NamedRoutes EnvironmentPage.Routes
  , _refresh :: mode :- "refresh" Servant.API.:> Refresh.StreamRoute
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.GlobalSettings -> App.ViraRuntimeState -> App.WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _home =
        Web.runAppInServant globalSettings viraRuntimeState webSettings $
          runAppHtml indexView
    , _repos = RegistryPage.handlers globalSettings viraRuntimeState webSettings
    , _jobs = JobPage.handlers globalSettings viraRuntimeState webSettings
    , _environment = EnvironmentPage.handlers globalSettings viraRuntimeState webSettings
    , _refresh =
        Web.runStreamHandler globalSettings viraRuntimeState Refresh.streamRouteHandler
    }

indexView :: AppHtml ()
indexView = do
  logoUrl <- W.appLogoUrl
  recentJobs <- lift $ App.query (St.GetRecentJobsA 10)
  let linkText = show . linkURI
      reposLink = linkText $ fieldLink _repos // RegistryPage._listing
      envLink = linkText $ fieldLink _environment // EnvironmentPage._view
  W.layout mempty $ do
    heroWelcome logoUrl reposLink envLink
    unless (null recentJobs) $ do
      viewRecentJobs recentJobs

viewRecentJobs :: [St.Job] -> AppHtml ()
viewRecentJobs jobs = do
  W.viraSection_ [] $ do
    h2_ [class_ "text-2xl font-bold text-gray-900 dark:text-gray-100 mb-6"] "Recent Jobs"
    div_ [class_ "space-y-3"] $ do
      forM_ jobs $ \job -> do
        branchUrl <- lift $ getLinkUrl $ LinkTo.RepoBranch job.repo job.branch
        div_ [class_ "space-y-1"] $ do
          -- Context header: repo → branch (no commit info - redundant with job row)
          W.viraJobContextHeader_ branchUrl $ do
            span_ $ toHtml $ toString job.repo
            span_ [class_ "mx-2 text-gray-500 dark:text-gray-400"] "→"
            span_ $ toHtml $ toString job.branch
          -- Job row
          W.viraJobRow_ Nothing job

heroWelcome :: (Monad m) => Text -> Text -> Text -> HtmlT m ()
heroWelcome logoUrl reposLink envLink = do
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
      a_ [href_ reposLink, class_ "flex-1 text-center py-2 px-4 bg-indigo-600 dark:bg-indigo-700 text-white font-semibold rounded-lg hover:bg-indigo-700 dark:hover:bg-indigo-600 transition-colors"] "Repositories"
      a_ [href_ envLink, class_ "flex-1 text-center py-2 px-4 bg-indigo-600 dark:bg-indigo-700 text-white font-semibold rounded-lg hover:bg-indigo-700 dark:hover:bg-indigo-600 transition-colors"] "Environment"
