{- |
Pull request display widgets.

Reusable badges and indicators for pull request state, fork status, etc.
-}
module Vira.Web.Widgets.PullRequest (
  forkBadge_,
  pullRequestStateBadge_,
) where

import Lucid
import Vira.State.Type (OwnerName (..), PullRequest (..), PullRequestState (..))
import Vira.State.Type qualified as St
import Web.TablerIcons.Outline qualified as Icon

-- | Fork indicator badge (shows head owner when PR is from a fork)
forkBadge_ :: (Monad m) => PullRequest -> HtmlT m ()
forkBadge_ pr
  | St.pullRequestIsFork pr =
      span_ [class_ "inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-purple-100 dark:bg-purple-900/30 text-purple-700 dark:text-purple-300"] $ do
        div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.git_fork
        toHtml $ "fork: " <> unOwnerName pr.headOwner
  | otherwise = mempty

-- | Icon for PR state
pullRequestStateIcon :: PullRequestState -> ByteString
pullRequestStateIcon = \case
  PullRequestOpen -> Icon.git_pull_request
  PullRequestClosed -> Icon.git_pull_request_closed
  PullRequestMerged -> Icon.git_merge

-- | Badge for PR state
pullRequestStateBadge_ :: (Monad m) => PullRequestState -> HtmlT m ()
pullRequestStateBadge_ prState = do
  let (colorClass, label) = case prState of
        PullRequestOpen -> ("bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300", "Open" :: Text)
        PullRequestClosed -> ("bg-red-100 dark:bg-red-900/30 text-red-800 dark:text-red-300", "Closed")
        PullRequestMerged -> ("bg-purple-100 dark:bg-purple-900/30 text-purple-800 dark:text-purple-300", "Merged")
  span_ [class_ $ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium " <> colorClass] $ do
    div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw $ pullRequestStateIcon prState
    toHtml label
