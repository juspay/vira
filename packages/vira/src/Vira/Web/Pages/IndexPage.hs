{-# LANGUAGE OverloadedRecordDot #-}

-- | Top-level routes and views
module Vira.Web.Pages.IndexPage where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Effectful.Git (Commit (..))
import Htmx.Lucid.Core (hxPost_, hxSwapS_)
import Htmx.Swap (Swap (..))
import Lucid
import Servant.API (Get, NamedRoutes, QueryParam, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (fieldLink, linkURI)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.GitHub.CheckRun qualified as CheckRun
import Vira.State.Acid qualified as St
import Vira.State.Type (BranchDetails, BranchQuery (..), Job (..), PRCommit (..), branchActivityTime, jobEndTime)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl, runAppHtml)
import Vira.Web.Pages.CachePage qualified as CachePage
import Vira.Web.Pages.EnvironmentPage qualified as EnvironmentPage
import Vira.Web.Pages.EventsPage qualified as EventsPage
import Vira.Web.Pages.JobPage qualified as JobPage
import Vira.Web.Pages.RegistryPage qualified as RegistryPage
import Vira.Web.Servant ((//))
import Vira.Web.Stack qualified as Web
import Vira.Web.Stream.ScopedRefresh qualified as Refresh
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Status qualified as Status
import Vira.Web.Widgets.Tabs (TabItem (..), viraTabs_)
import Vira.Web.Widgets.Time qualified as Time
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- QueryParam "tab" Text :> Get '[HTML] (Html ())
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

indexView :: Maybe Text -> AppHtml ()
indexView mTab = do
  logoUrl <- W.appLogoUrl
  let linkText = show . linkURI
      reposLink = linkText $ fieldLink _repos // RegistryPage._listing
      envLink = linkText $ fieldLink _environment // EnvironmentPage._view
      cacheLink = linkText $ fieldLink _cache // CachePage._view
  W.layout mempty $ do
    heroWelcome logoUrl reposLink envLink cacheLink
    viewRecentActivity mTab

-- | A unified activity item for interleaving branch and PR activity
data ActivityItem
  = BranchActivity BranchDetails
  | PRJobActivity Job [PRCommit]

activityTime :: ActivityItem -> UTCTime
activityTime = \case
  BranchActivity details -> branchActivityTime details
  PRJobActivity job _ -> job.jobCreatedTime

{- | Render a PR activity row in the same format as 'W.viraBranchDetailsRow_'.

If unapproved commits exist, shows the latest one with an Approve button
(like the Build button for NeverBuilt branches). Otherwise shows the
job with status badge.
-}
viraPRJobRow_ :: Job -> [PRCommit] -> AppHtml ()
viraPRJobRow_ job unapproved = do
  repoUrl <- lift $ getLinkUrl $ LinkTo.Repo job.repo
  let prNum = fromMaybe 0 job.prNumber
  prUrl <- lift $ getLinkUrl $ LinkTo.RepoPull job.repo prNum

  div_ [class_ "relative mb-6"] $ do
    -- Tags: purple repo + blue PR
    div_ [class_ "absolute -top-3 left-3 flex items-center z-10"] $ do
      a_ [href_ repoUrl, class_ "flex items-center gap-1 px-3 py-1 bg-purple-100 dark:bg-purple-900 border border-purple-300 dark:border-purple-700 rounded-l-full border-r-0 shadow-sm hover:opacity-70 transition-opacity"] $ do
        div_ [class_ "w-4 h-4 flex items-center justify-center text-purple-700 dark:text-purple-200 shrink-0"] $ toHtmlRaw Icon.book_2
        span_ [class_ "text-sm font-semibold text-purple-900 dark:text-purple-100"] $ toHtml $ toString job.repo
      a_ [href_ prUrl, class_ "flex items-center gap-1 px-3 py-1 bg-blue-100 dark:bg-blue-900 border border-blue-300 dark:border-blue-700 rounded-r-full border-l-0 shadow-sm hover:opacity-70 transition-opacity"] $ do
        div_ [class_ "w-4 h-4 flex items-center justify-center text-blue-700 dark:text-blue-200 shrink-0"] $ toHtmlRaw Icon.git_pull_request
        span_ [class_ "text-sm font-semibold text-blue-900 dark:text-blue-100"] $ toHtml $ "PR " <> show @Text prNum

    -- Card
    jobUrl <- lift $ getLinkUrl $ LinkTo.Job job.jobId
    a_
      [ href_ jobUrl
      , class_ "block pt-6 pb-4 px-4 rounded-lg bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 border-2 border-gray-200 dark:border-gray-700 transition-all cursor-pointer"
      ]
      $ do
        div_ [class_ "grid grid-cols-1 lg:grid-cols-12 gap-3 items-center"] $ do
          case unapproved of
            (pc : _) -> do
              -- Unapproved commit: show commit info + Approve button
              maybeCommit <- lift $ App.query $ St.GetCommitByIdA pc.sha
              div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
                W.viraCommitHash_ pc.sha
                whenJust maybeCommit $ \commit ->
                  unless (T.null commit.message) $ do
                    span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                    span_
                      [ class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"
                      , title_ commit.message
                      ]
                      $ toHtml commit.message
              div_ [class_ "lg:col-span-4 flex items-center justify-start lg:justify-end gap-2 flex-wrap"] $ do
                let approveLink = CheckRun.approvalUrl job.repo prNum pc.sha
                W.viraButton_
                  W.ButtonSuccess
                  [ hxPost_ approveLink
                  , hxSwapS_ AfterEnd
                  , onclick_ "event.preventDefault(); event.stopPropagation();"
                  , class_ "!px-3 !py-1.5 !text-xs"
                  ]
                  $ do
                    W.viraButtonIcon_ $ toHtmlRaw Icon.shield_check
                    "Approve"
            [] -> do
              -- All approved: show job info + status
              maybeCommit <- lift $ App.query $ St.GetCommitByIdA job.commit
              div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
                W.viraCommitHash_ job.commit
                whenJust maybeCommit $ \commit ->
                  unless (T.null commit.message) $ do
                    span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                    span_
                      [ class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"
                      , title_ commit.message
                      ]
                      $ toHtml commit.message
                span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                div_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
                  Time.viraRelativeTime_ job.jobCreatedTime
              div_ [class_ "lg:col-span-4 flex items-center justify-start lg:justify-end gap-2 flex-wrap"] $ do
                span_ [class_ "text-sm text-gray-600 dark:text-gray-400"] $ "#" <> toHtml (show @Text job.jobId)
                span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                case jobEndTime job of
                  Just endTime -> Time.viraDuration_ $ diffUTCTime endTime job.jobCreatedTime
                  Nothing -> mempty
                Status.viraStatusBadge_ job.jobStatus

viewRecentActivity :: Maybe Text -> AppHtml ()
viewRecentActivity mTab = do
  -- Fetch PR activities
  let fetchPRActivities = do
        recentJobs <- lift $ App.query (St.GetRecentJobsA activityLimit)
        let prJobs = filter (isJust . (.prNumber)) recentJobs
        forM prJobs $ \job -> do
          let prNum = fromMaybe 0 job.prNumber
          unapproved <- lift $ App.query (St.GetUnapprovedPRCommitsA job.repo prNum)
          pure $ PRJobActivity job unapproved

  -- Build activity list based on selected tab
  limited <- case mTab of
    Just "prs" ->
      take (fromIntegral activityLimit) . sortWith (Down . activityTime)
        <$> fetchPRActivities
    Just "unbuilt" -> do
      let query = def {neverBuilt = Just True}
      branchActivities <- lift $ App.query (St.QueryBranchDetailsA query activityLimit)
      pure $ map BranchActivity branchActivities
    Just "builds" -> do
      let query = def {neverBuilt = Just False}
      branchActivities <- lift $ App.query (St.QueryBranchDetailsA query activityLimit)
      pure $ map BranchActivity branchActivities
    _ -> do
      -- "All" tab: merge branches + PR jobs
      branchActivities <- lift $ App.query (St.QueryBranchDetailsA def activityLimit)
      prActivities <- fetchPRActivities
      pure $
        take (fromIntegral activityLimit) $
          sortWith (Down . activityTime) $
            map BranchActivity branchActivities <> prActivities

  -- Counts for badges
  let unbuiltQuery = def {neverBuilt = Just True}
  unbuiltCount <- length <$> lift (App.query (St.QueryBranchDetailsA unbuiltQuery activityLimit))
  prCount <- do
    recentJobs <- lift $ App.query (St.GetRecentJobsA activityLimit)
    pure $ length $ filter (isJust . (.prNumber)) recentJobs

  W.viraSection_ [] $ do
    h2_ [class_ "text-2xl font-bold text-gray-900 dark:text-gray-100"] "Recent Activity"

    -- Tab bar
    allUrl <- lift $ getLinkUrl (LinkTo.Home Nothing)
    buildsUrl <- lift $ getLinkUrl (LinkTo.Home (Just "builds"))
    unbuiltUrl <- lift $ getLinkUrl (LinkTo.Home (Just "unbuilt"))
    prsUrl <- lift $ getLinkUrl (LinkTo.Home (Just "prs"))
    viraTabs_
      []
      [ TabItem "All" allUrl (isNothing mTab) Nothing
      , TabItem "Builds" buildsUrl (mTab == Just "builds") Nothing
      , TabItem "Unbuilt" unbuiltUrl (mTab == Just "unbuilt") (Just unbuiltCount)
      , TabItem "PRs" prsUrl (mTab == Just "prs") (Just prCount)
      ]

    -- Activity list
    div_ $ do
      forM_ limited $ \case
        BranchActivity details ->
          W.viraBranchDetailsRow_ True details
        PRJobActivity job unapproved ->
          viraPRJobRow_ job unapproved

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
