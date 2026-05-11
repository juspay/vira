{-# LANGUAGE OverloadedRecordDot #-}

-- | Commit display components
module Vira.Web.Widgets.Commit (
  resolveBranchPullRequest,
  viraCommitInfo_,
  viraCommitInfoCompact_,
  viraCommitHash_,
  viewPullRequestLink_,
  viewPullRequestLookup_,
) where

import Colog (Severity (..))
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Effectful (Eff)
import Effectful.Git qualified as Git
import Effectful.Reader.Dynamic qualified as Reader
import GH.Core qualified as GH
import Lucid
import Vira.App qualified
import Vira.Lib.TimeExtra (formatRelativeTime)
import Vira.State.Acid qualified
import Vira.Web.Lucid (AppHtml)
import Vira.Web.PullRequestCache qualified as PullRequestCache
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Code qualified as Code
import Web.TablerIcons.Outline qualified as Icon

-- | Look up the GitHub pull request associated with a branch.
resolveBranchPullRequest :: Text -> Git.BranchName -> Eff Web.AppServantStack GH.PullRequestLookup
resolveBranchPullRequest cloneUrl branchName = do
  cache <- Reader.asks @Vira.App.ViraRuntimeState (.pullRequestCache)
  cachedLookup <- liftIO $ PullRequestCache.resolvePullRequest cache cloneUrl branchName
  case (cachedLookup.source, cachedLookup.result) of
    (PullRequestCache.PullRequestCacheMiss, GH.PullRequestLookupFailed err) ->
      Vira.App.log Warning $ "Could not look up GitHub pull request for branch " <> toText branchName <> ": " <> err
    _ -> pass
  pure cachedLookup.result

-- | Render a pull request lookup result as a link when a PR exists.
viewPullRequestLookup_ :: (Monad m) => GH.PullRequestLookup -> HtmlT m ()
viewPullRequestLookup_ = \case
  GH.FoundPullRequest pullRequest -> viewPullRequestLink_ pullRequest
  GH.UnsupportedRepository -> mempty
  GH.NoPullRequest -> mempty
  GH.PullRequestLookupFailed _ -> mempty

-- | Render an external link to a GitHub pull request.
viewPullRequestLink_ :: (Monad m) => GH.PullRequest -> HtmlT m ()
viewPullRequestLink_ pullRequest =
  a_
    [ href_ pullRequest.url
    , target_ "_blank"
    , rel_ "noopener noreferrer"
    , title_ pullRequest.title
    , class_ "inline-flex items-center gap-1.5 px-3 py-1.5 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-sm font-semibold text-gray-700 dark:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors"
    ]
    $ do
      span_ [class_ "w-4 h-4 flex items-center justify-center"] $
        toHtmlRaw Icon.git_pull_request
      span_ $ "PR #" <> toHtml (show @Text pullRequest.number)

-- | Commit info display: hash, message, author, date
viraCommitInfo_ :: Git.CommitID -> AppHtml ()
viraCommitInfo_ commitId = do
  maybeCommit <- lift $ Vira.App.query $ Vira.State.Acid.GetCommitByIdA commitId
  div_ [class_ "flex items-center space-x-2 min-w-0"] $ do
    viraCommitHash_ commitId
    case maybeCommit of
      Just commit -> do
        unless (T.null commit.message) $ do
          span_ [class_ "text-sm text-gray-600 dark:text-gray-300 truncate min-w-0 max-w-sm"] $ toHtml commit.message
        unless (T.null commit.author) $ do
          span_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $ do
            "by " <> toHtml commit.author
            unless (T.null commit.authorEmail) $ do
              " <" <> toHtml commit.authorEmail <> ">"
        div_ [class_ "text-xs text-gray-400 dark:text-gray-500"] $
          toHtml $
            formatTime defaultTimeLocale "%b %d, %Y" commit.date
      Nothing -> do
        span_ [class_ "text-xs text-red-600 dark:text-red-400"] "Commit not found"

-- | Compact commit info: hash, message, relative time
viraCommitInfoCompact_ :: Maybe Git.Commit -> AppHtml ()
viraCommitInfoCompact_ mCommit = do
  now <- liftIO getCurrentTime
  div_ [class_ "flex items-baseline space-x-2"] $ do
    case mCommit of
      Just commit -> do
        viraCommitHash_ commit.id
        unless (T.null commit.message) $ do
          span_ [class_ "text-sm text-gray-700 dark:text-gray-300 truncate max-w-xs"] $ toHtml commit.message
        div_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
          toHtml $
            formatRelativeTime now commit.date
      Nothing -> do
        viraCommitHash_ (Git.CommitID "unknown")
        span_ [class_ "text-xs text-red-600 dark:text-red-400"] "Commit not found"

-- | Clickable 'Effectful.Git.Types.CommitID' hash (8 chars) with copy-to-clipboard
viraCommitHash_ :: Git.CommitID -> AppHtml ()
viraCommitHash_ commitId = do
  let shortHash = T.take 8 $ toText commitId
      fullHash = toText commitId
  Code.viraCodeInlineCopyable shortHash fullHash
