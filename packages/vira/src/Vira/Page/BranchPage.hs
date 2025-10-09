{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.BranchPage where

import Control.Concurrent.STM (readTVarIO)
import Data.Map qualified as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName, RepoName)
import Effectful.Reader.Dynamic (asks)
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.TimeExtra (formatRelativeTime)
import Vira.Refresh.Type (RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Commit qualified as W
import Vira.Widgets.Layout qualified as W
import Vira.Widgets.Status qualified as Status
import Vira.Widgets.Time qualified as Time
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.AppState -> WebSettings -> RepoName -> BranchName -> Routes AsServer
handlers globalSettings appState webSettings repoName branchName = do
  Routes
    { _view = App.runAppInServant globalSettings appState webSettings . App.runAppHtml $ viewHandler repoName branchName
    }

viewHandler :: RepoName -> BranchName -> AppHtml ()
viewHandler repoName branchName = do
  repo <- lift $ App.query (St.GetRepoByNameA repoName) >>= maybe (throwError err404) pure
  branch <- lift $ App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError err404) pure
  jobs <- lift $ App.query $ St.GetJobsByBranchA repoName branchName
  let branchCrumbs = [LinkTo.RepoListing, LinkTo.Repo repoName, LinkTo.RepoBranch repoName branchName]
  W.layout branchCrumbs $ viewBranch repo branch jobs

viewBranch :: St.Repo -> St.Branch -> [St.Job] -> App.AppHtml ()
viewBranch repo branch jobs = do
  -- Branch header with refresh button
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.git_branch)
    (toText $ toString repo.name <> " → " <> toString branch.branchName)
    ( div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "flex items-center space-x-3 text-gray-600 dark:text-gray-300 min-w-0 flex-1"] $ do
          span_ [class_ "text-sm shrink-0"] "Latest commit:"
          div_ [class_ "flex items-center space-x-2 min-w-0"] $ do
            div_ [class_ "w-4 h-4 flex items-center justify-center shrink-0"] $ toHtmlRaw Icon.git_commit
            div_ [class_ "min-w-0"] $ W.viraCommitInfo_ branch.headCommit
        div_ [class_ "flex items-center gap-2"] $ do
          viewRefreshStatus repo
          buildLink <- lift $ App.getLink $ LinkTo.Build repo.name branch.branchName
          updateLink <- lift $ App.getLink $ LinkTo.RepoUpdate repo.name
          W.viraRequestButton_
            W.ButtonPrimary
            buildLink
            [title_ "Build this branch"]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.player_play
              "Build"
          W.viraRequestButton_
            W.ButtonSecondary
            updateLink
            [title_ "Refresh branches"]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              "Refresh"
    )

  W.viraSection_ [] $ do
    div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
      viewCommitTimeline branch jobs

-- Simple job list showing commit id, job id, and status
viewCommitTimeline :: St.Branch -> [St.Job] -> App.AppHtml ()
viewCommitTimeline branch jobs = do
  div_ [class_ "space-y-3"] $ do
    -- Show current branch commit if no jobs exist
    when (null jobs) $ do
      div_ [class_ "flex items-center p-3 rounded-lg bg-gray-50 dark:bg-gray-700 border border-gray-200 dark:border-gray-600"] $ do
        div_ [class_ "w-5 h-5 mr-3 flex items-center justify-center text-gray-500 dark:text-gray-400"] $ toHtmlRaw Icon.git_commit
        div_ [class_ "flex-1"] $ do
          div_ [class_ "flex items-center space-x-4"] $ do
            W.viraCommitInfo_ branch.headCommit
            span_ [class_ "text-sm text-gray-500 dark:text-gray-400"] "No builds yet"

    -- Show all jobs for this branch
    forM_ jobs $ \job -> do
      maybeCommit <- lift $ App.query $ St.GetCommitByIdA job.commit
      jobUrl <- lift $ App.getLinkUrl $ LinkTo.Job job.jobId
      a_ [href_ jobUrl, class_ "block p-3 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 border border-gray-200 dark:border-gray-700 transition-colors"] $ do
        -- Single-line columnar layout for easy scanning
        div_ [class_ "grid grid-cols-12 gap-4 items-center"] $ do
          -- Column 1: Job ID (2 columns)
          div_ [class_ "col-span-2 flex items-center space-x-2"] $ do
            div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-600 dark:text-gray-400"] $ toHtmlRaw Icon.git_commit
            span_ [class_ "text-sm font-semibold text-gray-900 dark:text-gray-100"] $ "#" <> toHtml (show @Text job.jobId)

          -- Column 2: Commit info (6 columns)
          div_ [class_ "col-span-6 min-w-0"] $ do
            W.viraCommitInfoCompact_ maybeCommit

          -- Column 3: Build duration and status (4 columns)
          div_ [class_ "col-span-4 flex items-center justify-end space-x-2"] $ do
            -- Build duration
            case St.jobEndTime job of
              Just endTime -> do
                let duration = diffUTCTime endTime job.jobCreatedTime
                Time.viraDuration_ duration
              Nothing -> mempty
            -- Status badge
            Status.viraStatusBadge_ job.jobStatus

-- | View the refresh status for a repository
viewRefreshStatus :: St.Repo -> App.AppHtml ()
viewRefreshStatus repo = do
  currentTime <- liftIO getCurrentTime
  statusMap <- lift $ asks @App.AppState (.refreshStatuses) >>= liftIO . Control.Concurrent.STM.readTVarIO
  let refreshStatus = Map.lookup repo.name statusMap

  div_ [class_ "text-sm text-gray-500 dark:text-gray-400 mr-3"] $ do
    case refreshStatus of
      Just (RefreshSuccess completedAt) -> do
        span_ [class_ "text-green-600 dark:text-green-400"] "✓"
        " "
        "Refreshed "
        toHtml $ formatRelativeTime currentTime completedAt
      Just (RefreshFailure err completedAt) -> do
        span_ [class_ "text-red-600 dark:text-red-400", title_ err] "⚠"
        " Refresh failed "
        toHtml $ formatRelativeTime currentTime completedAt
      Just RefreshPending {} -> do
        span_ [class_ "text-blue-600 dark:text-blue-400"] "⟳"
        " Refreshing..."
      Just RefreshNotStarted -> do
        span_ [class_ "text-gray-600 dark:text-gray-400"] "○"
        " Not refreshed"
      Nothing -> do
        span_ [class_ "text-gray-600 dark:text-gray-400"] "○"
        " Not refreshed"
