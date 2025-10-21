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
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Status qualified as W
import Vira.Web.Widgets.Time qualified as Time
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
        maybeCommit <- lift $ App.query $ St.GetCommitByIdA job.commit
        jobUrl <- lift $ getLinkUrl $ LinkTo.Job job.jobId
        repoUrl <- lift $ getLinkUrl $ LinkTo.Repo job.repo
        branchUrl <- lift $ getLinkUrl $ LinkTo.RepoBranch job.repo job.branch
        W.viraCard_ [class_ "p-4 hover:shadow-lg transition-shadow group relative"] $ do
          a_ [href_ jobUrl, class_ "absolute inset-0 z-0"] ""
          -- Use grid for proper column alignment (pointer-events-none allows clicks through to overlay)
          div_ [class_ "grid grid-cols-12 gap-4 items-center relative z-10 pointer-events-none"] $ do
            -- Column 1: Job ID (1 col)
            div_ [class_ "col-span-1"] $ do
              span_ [class_ "text-sm font-semibold text-gray-900 dark:text-gray-100 whitespace-nowrap group-hover:text-indigo-600 dark:group-hover:text-indigo-400"] $ "#" <> toHtml (show @Text job.jobId)

            -- Column 2: Repo / Branch (3 cols, vertically stacked)
            div_ [class_ "col-span-3"] $ do
              div_ [class_ "flex flex-col space-y-0.5"] $ do
                a_ [href_ repoUrl, class_ "font-semibold text-sm text-gray-900 dark:text-gray-100 hover:text-indigo-600 dark:hover:text-indigo-400 truncate relative z-20 pointer-events-auto"] $ toHtml $ toString job.repo
                a_ [href_ branchUrl, class_ "text-xs text-gray-500 dark:text-gray-400 hover:text-indigo-600 dark:hover:text-indigo-400 truncate relative z-20 pointer-events-auto"] $ toHtml $ toString job.branch

            -- Column 3: Commit info (5 cols)
            div_ [class_ "col-span-5 min-w-0"] $ do
              W.viraCommitInfoCompact_ maybeCommit

            -- Column 4: Time (2 cols)
            div_ [class_ "col-span-2 text-xs text-gray-500 dark:text-gray-400"] $ do
              Time.viraRelativeTime_ job.jobCreatedTime

            -- Column 5: Status (1 col)
            div_ [class_ "col-span-1 flex justify-end"] $ do
              W.viraStatusBadge_ job.jobStatus

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
