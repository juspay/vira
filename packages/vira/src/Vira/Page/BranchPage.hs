{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.BranchPage where

import Data.Text qualified as T
import Effectful.Error.Static (throwError)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Layout qualified as W
import Vira.Widgets.Status qualified as Status
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.AppState -> RepoName -> BranchName -> Routes AsServer
handlers cfg repoName branchName = do
  Routes
    { _view = App.runAppInServant cfg . App.runAppHtml $ viewHandler repoName branchName
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
        div_ [class_ "flex items-center space-x-3 text-gray-600"] $ do
          span_ [class_ "text-sm"] "Latest commit:"
          div_ [class_ "flex items-center space-x-2"] $ do
            div_ [class_ "w-4 h-4 flex items-center justify-center"] $ toHtmlRaw Icon.git_commit
            viewCommitHash branch.headCommit
        div_ [class_ "flex items-center gap-2"] $ do
          buildLink <- lift $ App.getLink $ LinkTo.Build repo.name branch.branchName
          updateLink <- lift $ App.getLink $ LinkTo.RepoUpdate repo.name
          W.viraButton_
            W.ButtonPrimary
            [ hxPostSafe_ buildLink
            , hxSwapS_ AfterEnd
            , title_ "Build this branch"
            ]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.player_play
              "Build"
          W.viraButton_
            W.ButtonSecondary
            [ hxPostSafe_ updateLink
            , hxSwapS_ AfterEnd
            , title_ "Refresh branches"
            ]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              "Refresh"
    )

  W.viraSection_ [] $ do
    div_ [class_ "bg-white rounded-xl border border-gray-200 p-4 lg:p-8"] $ do
      viewCommitTimeline branch jobs

-- Timeline commit hash display without background
viewCommitHash :: (Monad m) => Git.CommitID -> HtmlT m ()
viewCommitHash (Git.CommitID commit) = do
  span_ [class_ "font-mono text-sm text-gray-500 font-medium"] $
    toHtml $
      T.take 8 $
        toText commit

-- Simple job list showing commit id, job id, and status
viewCommitTimeline :: St.Branch -> [St.Job] -> App.AppHtml ()
viewCommitTimeline branch jobs = do
  div_ [class_ "space-y-3"] $ do
    -- Show current branch commit if no jobs exist
    when (null jobs) $ do
      div_ [class_ "flex items-center p-3 rounded-lg bg-gray-50 border border-gray-200"] $ do
        div_ [class_ "w-5 h-5 mr-3 flex items-center justify-center text-gray-500"] $ toHtmlRaw Icon.git_commit
        div_ [class_ "flex-1"] $ do
          div_ [class_ "flex items-center space-x-4"] $ do
            viewCommitHash branch.headCommit
            span_ [class_ "text-sm text-gray-500"] "No builds yet"

    -- Show all jobs for this branch
    forM_ jobs $ \job -> do
      jobUrl <- lift $ App.getLinkUrl $ LinkTo.Job job.jobId
      a_ [href_ jobUrl, class_ "block p-3 rounded-lg hover:bg-gray-50 border border-gray-200 transition-colors"] $ do
        div_ [class_ "flex items-center space-x-4"] $ do
          div_ [class_ "w-5 h-5 flex items-center justify-center text-gray-500"] $ toHtmlRaw Icon.git_commit
          viewCommitHash job.jobCommit
          span_ [class_ "text-sm font-medium text-gray-600"] $ "#" <> toHtml (show @Text job.jobId)
          div_ [class_ "ml-auto"] $ do
            Status.viraStatusBadge_ job.jobStatus
