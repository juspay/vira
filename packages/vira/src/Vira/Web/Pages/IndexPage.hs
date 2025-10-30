{-# LANGUAGE OverloadedRecordDot #-}

-- | Top-level routes and views
module Vira.Web.Pages.IndexPage where

import Effectful.Git (Commit (..))
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
import Vira.Web.Pages.CachePage qualified as CachePage
import Vira.Web.Pages.EnvironmentPage qualified as EnvironmentPage
import Vira.Web.Pages.JobPage qualified as JobPage
import Vira.Web.Pages.RegistryPage qualified as RegistryPage
import Vira.Web.Servant ((//))
import Vira.Web.Stack qualified as Web
import Vira.Web.Stream.ScopedRefresh qualified as Refresh
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _environment :: mode :- "env" Servant.API.:> NamedRoutes EnvironmentPage.Routes
  , _cache :: mode :- "cache" Servant.API.:> NamedRoutes CachePage.Routes
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
    , _cache = CachePage.handlers globalSettings viraRuntimeState webSettings
    , _refresh =
        Web.runStreamHandler globalSettings viraRuntimeState . Refresh.streamRouteHandler
    }

indexView :: AppHtml ()
indexView = do
  logoUrl <- W.appLogoUrl
  activities <- lift $ App.query (St.GetRecentActivityA 15)
  let linkText = show . linkURI
      reposLink = linkText $ fieldLink _repos // RegistryPage._listing
      envLink = linkText $ fieldLink _environment // EnvironmentPage._view
      cacheLink = linkText $ fieldLink _cache // CachePage._view
  W.layout mempty $ do
    heroWelcome logoUrl reposLink envLink cacheLink
    unless (null activities) $
      viewRecentActivity activities

viewRecentActivity :: [St.BranchDetails] -> AppHtml ()
viewRecentActivity activities = do
  W.viraSection_ [] $ do
    h2_ [class_ "text-2xl font-bold text-gray-900 dark:text-gray-100 mb-6"] "Recent Activity"
    div_ [class_ "space-y-3"] $ do
      forM_ activities $ \details -> do
        branchUrl <- lift $ getLinkUrl $ LinkTo.RepoBranch details.branch.repoName details.branch.branchName
        let isOutOfDate = case details.mLatestJob of
              Nothing -> True
              Just job -> details.branch.headCommit.id /= job.commit

        div_ [class_ "space-y-1"] $ do
          -- Branch header: repo → branch with status badge and commit info
          W.viraJobContextHeader_ branchUrl $ do
            div_ [class_ "flex items-center justify-between"] $ do
              -- Left: repo → branch with out-of-date badge
              div_ [class_ "flex items-center space-x-2"] $ do
                div_ [class_ "w-4 h-4 flex items-center justify-center"] $ toHtmlRaw Icon.book_2
                span_ $ toHtml $ toString details.branch.repoName
                span_ [class_ "mx-2 opacity-50 group-hover:opacity-100"] "→"
                div_ [class_ "w-5 h-5 flex items-center justify-center opacity-50 group-hover:opacity-100"] $ toHtmlRaw Icon.git_branch
                span_ [class_ "opacity-50 group-hover:opacity-100"] $ toHtml $ toString details.branch.branchName
                when isOutOfDate $ do
                  span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300"] $ do
                    div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.clock
                    "Out of date"
              -- Right: commit info
              div_ [class_ "text-xs opacity-50 group-hover:opacity-100 flex items-center space-x-3"] $ do
                W.viraCommitInfoCompact_ (Just details.branch.headCommit)
                when (details.jobsCount > 0) $ do
                  span_ $ "(" <> toHtml (show @Text details.jobsCount) <> " builds)"

          -- Job row - only if job exists
          whenJust details.mLatestJob $ \latestJob -> do
            div_ [class_ "ml-7"] $ do
              W.viraJobRow_ Nothing latestJob

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
