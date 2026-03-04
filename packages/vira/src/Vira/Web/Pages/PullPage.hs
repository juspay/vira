{-# LANGUAGE OverloadedRecordDot #-}

{- | Pull request detail page with commit history and approval

The approval route is handled by Vira.GitHub.Middleware for GitHub integration.
This module only handles the PR detail view.
-}
module Vira.Web.Pages.PullPage (
  Routes (..),
  handlers,

  -- * Shared widgets
  forkBadge_,
  approveButton_,
  prStateBadge_,
) where

import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName (..), CommitID, RepoName (..))
import Htmx.Lucid.Core (hxPost_)
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.GitHub.CheckRun qualified as CheckRun
import Vira.State.Acid qualified as St
import Vira.State.Type (PRCommit (..), PRState (..), PullRequest (..))
import Vira.State.Type qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

newtype Routes mode = Routes
  { _detail :: mode :- Capture "number" Int :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings repoName =
  Routes
    { _detail = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . detailHandler repoName
    }

-- * Detail

detailHandler :: RepoName -> Int -> AppHtml ()
detailHandler repoName prNum = do
  pr <- lift $ App.query (St.GetPullRequestA repoName prNum) >>= maybe (throwError err404) pure
  commits <- lift $ App.query $ St.GetPRCommitsByPRA repoName prNum
  let branchRef = BranchName $ "refs/pull/" <> show prNum <> "/head"
  jobs <- lift $ App.query $ St.GetJobsByBranchA repoName branchRef
  let crumbs =
        [ LinkTo.RepoListing
        , LinkTo.Repo repoName
        , LinkTo.RepoPull repoName prNum
        ]
  W.layout crumbs $ viewPRDetail pr commits jobs

viewPRDetail :: PullRequest -> [PRCommit] -> [St.Job] -> AppHtml ()
viewPRDetail pr commits jobs = do
  let ghPrUrl = "https://github.com/" <> unRepoName pr.repoName <> "/pull/" <> show pr.prNumber
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.git_pull_request)
    (pr.title <> " #" <> show pr.prNumber)
    ( div_ [class_ "flex items-center space-x-3"] $ do
        div_ [class_ "flex items-center space-x-2 text-sm text-gray-600 dark:text-gray-300"] $ do
          span_ $ toHtml (unBranchName pr.headBranch) <> " → " <> toHtml (unBranchName pr.baseBranch)
        forkBadge_ pr.forkRepo
        prStateBadge_ pr.prState
        a_ [href_ ghPrUrl, target_ "blank", class_ "text-gray-400 hover:text-gray-600 dark:hover:text-gray-300"] $
          div_ [class_ "w-5 h-5 flex items-center justify-center"] $
            toHtmlRaw Icon.brand_github
    )

  W.viraSection_ [] $ do
    div_ [class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 p-4 lg:p-8"] $ do
      -- Unapproved commits awaiting approval
      let unapprovedWithoutJob = filter (\pc -> not pc.approved) commits
      forM_ unapprovedWithoutJob $ \pc ->
        viewUnapprovedCommitRow pr pc

      -- All jobs for this PR (newest first, already sorted by GetJobsByBranchA)
      if null jobs && null unapprovedWithoutJob
        then div_ [class_ "text-center py-8 text-gray-500 dark:text-gray-400"] "No commits tracked"
        else forM_ jobs $ \job ->
          W.viraJobRow_ Nothing job

-- | Unapproved fork commit: show commit info + approve button
viewUnapprovedCommitRow :: PullRequest -> PRCommit -> AppHtml ()
viewUnapprovedCommitRow pr pc =
  div_ [class_ "mb-6 p-4 rounded-lg bg-gray-50 dark:bg-gray-800 border-2 border-gray-200 dark:border-gray-700"] $ do
    div_ [class_ "flex items-center justify-between"] $ do
      div_ [class_ "flex items-center space-x-3 min-w-0"] $ do
        div_ [class_ "w-5 h-5 flex items-center justify-center shrink-0 text-yellow-500 dark:text-yellow-400"] $
          toHtmlRaw Icon.shield_check
        W.viraCommitInfo_ pc.sha
      approveButton_ pr.repoName pr.prNumber pc.sha

-- * UI Helpers

-- | Fork indicator badge
forkBadge_ :: (Monad m) => Maybe Text -> HtmlT m ()
forkBadge_ Nothing = mempty
forkBadge_ (Just repo) =
  span_ [class_ "inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-purple-100 dark:bg-purple-900/30 text-purple-700 dark:text-purple-300"] $ do
    div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.git_fork
    toHtml $ "fork: " <> repo

{- | Approve button for unapproved fork commits

Posts to GitHub approval route at @/github/r/:owner/:repo/pull/:num/approve/:sha@
-}
approveButton_ :: RepoName -> Int -> CommitID -> AppHtml ()
approveButton_ repoName prNum sha = do
  let approveLink = CheckRun.approvalUrl repoName prNum sha
  W.viraButton_
    W.ButtonSuccess
    [hxPost_ approveLink, class_ "text-xs px-3 py-1"]
    $ do
      W.viraButtonIcon_ $ toHtmlRaw Icon.shield_check
      "Approve"

-- | Icon for PR state
prStateIcon :: PRState -> ByteString
prStateIcon = \case
  PROpen -> Icon.git_pull_request
  PRClosed -> Icon.git_pull_request_closed
  PRMerged -> Icon.git_merge

-- | Badge for PR state
prStateBadge_ :: (Monad m) => PRState -> HtmlT m ()
prStateBadge_ prState = do
  let (colorClass, label) = case prState of
        PROpen -> ("bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300", "Open" :: Text)
        PRClosed -> ("bg-red-100 dark:bg-red-900/30 text-red-800 dark:text-red-300", "Closed")
        PRMerged -> ("bg-purple-100 dark:bg-purple-900/30 text-purple-800 dark:text-purple-300", "Merged")
  span_ [class_ $ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium " <> colorClass] $ do
    div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw $ prStateIcon prState
    toHtml label
