{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.BranchPage where

import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName, Commit (..), RepoName)
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (BranchBuildState (..), BranchDetails (..), BuildFreshness (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Status qualified as Status
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> BranchName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings repoName branchName = do
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler repoName branchName
    }

viewHandler :: RepoName -> BranchName -> AppHtml ()
viewHandler repoName branchName = do
  repo <- lift $ App.query (St.GetRepoByNameA repoName) >>= maybe (throwError err404) pure
  branchDetails <- lift $ App.query (St.GetBranchDetailsA repoName branchName) >>= maybe (throwError err404) pure
  jobs <- lift $ App.query $ St.GetJobsByBranchA repoName branchName
  let branchCrumbs = [LinkTo.RepoListing, LinkTo.Repo repoName, LinkTo.RepoBranch repoName branchName]
  W.layout branchCrumbs $ viewBranch repo branchDetails jobs

viewBranch :: St.Repo -> BranchDetails -> [St.Job] -> AppHtml ()
viewBranch repo branchDetails jobs = do
  -- Branch header with build and refresh buttons
  let branchTitle =
        toString repo.name
          <> " → "
          <> toString branchDetails.branch.branchName
          <> if branchDetails.branch.deleted then " (deleted)" else ""
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.git_branch)
    (toText branchTitle)
    ( div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "flex items-center space-x-3 text-gray-600 dark:text-gray-300 min-w-0 flex-1"] $ do
          span_ [class_ "text-sm shrink-0"] "Latest commit:"
          div_ [class_ "flex items-center space-x-2 min-w-0"] $ do
            div_ [class_ "w-4 h-4 flex items-center justify-center shrink-0"] $ toHtmlRaw Icon.git_commit
            div_ [class_ "min-w-0"] $ W.viraCommitInfo_ branchDetails.branch.headCommit.id
          -- Deleted badge
          when branchDetails.branch.deleted $
            span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-red-100 dark:bg-red-900/30 text-red-700 dark:text-red-300"] $ do
              div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.alert_triangle
              "Branch deleted from remote"
          -- Out of date badge
          case branchDetails.buildState of
            Built OutOfDate ->
              span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300"] $ do
                div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.clock
                "Out of date"
            _ -> mempty -- Don't show badge for NeverBuilt or UpToDate
        div_ [class_ "flex items-center gap-2"] $ do
          buildLink <- lift $ getLink $ LinkTo.Build repo.name branchDetails.branch.branchName
          W.viraRequestButton_
            W.ButtonSuccess
            buildLink
            [title_ "Build this branch"]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.player_play
              "Build"
          Status.viraSmartRefreshButton_ repo.name
    )

  W.viraSection_ [] $ do
    div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
      viewCommitTimeline branchDetails.branch jobs

-- Simple job list showing commit id, job id, and status
viewCommitTimeline :: St.Branch -> [St.Job] -> AppHtml ()
viewCommitTimeline branch jobs = do
  div_ [] $ do
    -- Show current branch commit if no jobs exist
    when (null jobs) $ do
      div_ [class_ "mb-6 flex items-center p-4 rounded-lg bg-gray-50 dark:bg-gray-800 border-2 border-gray-200 dark:border-gray-700"] $ do
        div_ [class_ "w-5 h-5 mr-3 flex items-center justify-center text-gray-500 dark:text-gray-400"] $ toHtmlRaw Icon.git_commit
        div_ [class_ "flex-1"] $ do
          div_ [class_ "flex items-center space-x-4"] $ do
            W.viraCommitInfoCompact_ (Just branch.headCommit)
            span_ [class_ "text-sm text-gray-500 dark:text-gray-400"] "No builds yet"

    -- Show all jobs for this branch
    forM_ jobs $ \job -> do
      W.viraJobRow_ Nothing job
