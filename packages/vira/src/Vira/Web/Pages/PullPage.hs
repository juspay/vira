{-# LANGUAGE OverloadedRecordDot #-}

{- | Pull request detail page with commit history and approval

Approval is a core Vira action (mark commit approved + enqueue job).
GitHub check run creation is handled reactively by 'Vira.GitHub.Webhook'.
-}
module Vira.Web.Pages.PullPage (
  Routes (..),
  handlers,

  -- * Shared widgets
  forkBadge_,
  prStateBadge_,
) where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
import Htmx.Servant.Response
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.CI.Client qualified as Client
import Vira.State.Acid qualified as St
import Vira.State.Type (ForgeInfo (..), OwnerName (..), PRCommit (..), PRState (..), PullRequest (..), prBranchRef)
import Vira.State.Type qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.JobsListing qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

data Routes mode = Routes
  { _detail :: mode :- Capture "number" Int :> Get '[HTML] (Html ())
  , _approve :: mode :- Capture "number" Int :> "approve" :> Capture "sha" CommitID :> Post '[HTML] (Headers '[HXRefresh] Text)
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> RepoName -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings repoName =
  Routes
    { _detail = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . detailHandler repoName
    , _approve = \prNum sha -> Web.runAppInServant globalSettings viraRuntimeState webSettings $ approveHandler repoName prNum sha
    }

-- * Detail

detailHandler :: RepoName -> Int -> AppHtml ()
detailHandler repoName prNum = do
  pr <- lift $ App.query (St.GetPullRequestA repoName prNum) >>= maybe (throwError err404) pure
  commits <- lift $ App.query $ St.GetPRCommitsByPRA repoName prNum
  let branchRef = St.prBranchRef prNum
  jobs <- lift $ App.query $ St.GetJobsByBranchA repoName branchRef
  let crumbs =
        [ LinkTo.RepoListing
        , LinkTo.Repo repoName
        , LinkTo.RepoPull repoName prNum
        ]
  W.layout crumbs $ viewPRDetail pr commits jobs

viewPRDetail :: PullRequest -> [PRCommit] -> [St.Job] -> AppHtml ()
viewPRDetail pr commits jobs = do
  W.viraPageHeaderWithIcon_
    (toHtmlRaw Icon.git_pull_request)
    (pr.title <> " #" <> show pr.prNumber)
    ( div_ [class_ "flex items-center space-x-3"] $ do
        div_ [class_ "flex items-center space-x-2 text-sm text-gray-600 dark:text-gray-300"] $ do
          span_ $ toHtml (unBranchName pr.headBranch) <> " → " <> toHtml (unBranchName pr.baseBranch)
        forkBadge_ pr
        prStateBadge_ pr.prState
        whenJust pr.forgeInfo $ \f ->
          a_ [href_ f.url, target_ "blank", class_ "text-gray-400 hover:text-gray-600 dark:hover:text-gray-300"] $
            div_ [class_ "w-5 h-5 flex items-center justify-center"] $
              toHtmlRaw f.icon
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

-- * Approval

{- | Handle fork PR approval: mark commit approved and enqueue job

This is core Vira logic with no GitHub dependency. GitHub check run
creation is handled reactively by 'Vira.GitHub.Webhook.prCheckRunWatcher'.
-}
approveHandler :: RepoName -> Int -> CommitID -> Eff Web.AppServantStack (Headers '[HXRefresh] Text)
approveHandler repoName prNum sha = do
  mCommit <- App.query $ St.GetPRCommitA repoName prNum sha
  case mCommit of
    Nothing -> throwError err404
    Just pc
      | pc.approved -> throwError err400
      | otherwise -> do
          void $ App.update $ St.ApprovePRCommitA repoName prNum sha
          let branchRef = prBranchRef prNum
          void $ Client.enqueueJob repoName branchRef pc.commit.id (Just prNum)
          pure $ addHeader True "Approved"

-- | Unapproved fork commit: show commit info + approve button
viewUnapprovedCommitRow :: PullRequest -> PRCommit -> AppHtml ()
viewUnapprovedCommitRow pr pc =
  div_ [class_ "mb-6 p-4 rounded-lg bg-gray-50 dark:bg-gray-800 border-2 border-gray-200 dark:border-gray-700"] $ do
    div_ [class_ "flex items-center justify-between"] $ do
      div_ [class_ "flex items-center space-x-3 min-w-0"] $ do
        div_ [class_ "w-5 h-5 flex items-center justify-center shrink-0 text-yellow-500 dark:text-yellow-400"] $
          toHtmlRaw Icon.shield_check
        W.viraCommitInfo_ pc.commit.id
      approveButton_ pr.repo pr.prNumber pc.commit.id

-- * UI Helpers

-- | Fork indicator badge (shows head owner when PR is from a fork)
forkBadge_ :: (Monad m) => PullRequest -> HtmlT m ()
forkBadge_ pr
  | St.prIsFork pr =
      span_ [class_ "inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-purple-100 dark:bg-purple-900/30 text-purple-700 dark:text-purple-300"] $ do
        div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.git_fork
        toHtml $ "fork: " <> unOwnerName pr.headOwner
  | otherwise = mempty

-- | Approve button for unapproved fork commits (posts to core approval route)
approveButton_ :: RepoName -> Int -> CommitID -> AppHtml ()
approveButton_ repoName prNum sha = do
  approveLink <- lift $ getLink $ LinkTo.PRApprove repoName prNum sha
  W.viraButton_
    W.ButtonSuccess
    [hxPostSafe_ approveLink, class_ "text-xs px-3 py-1"]
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
