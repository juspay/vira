{-# LANGUAGE OverloadedRecordDot #-}

{- |
Vira Design System - Status Components

This module contains status display components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Status Display Components
- 'viraStatusBadge_' - Status indicators with semantic colors

= Usage Guidelines

Use these components for displaying job status and other state information.
Colors are automatically managed based on the status type for consistency.

= Design Principles

- Semantic Colors: Clear visual status communication
- Type Safety: Compile-time status validation
- Accessibility: Proper contrast and readability
- Consistency: Unified status representation
-}
module Vira.Web.Widgets.Status (
  viraStatusBadge_,
  viraRefreshStatus_,
  viewAllJobStatus,
  indicator,
  statusLabel,
) where

import Effectful.Git (RepoName (..))
import Lucid
import Vira.App.AcidState qualified as App
import Vira.Lib.TimeExtra (formatDuration, formatTimestamp)
import Vira.Refresh.Type (RefreshState, RefreshStatus (..), getRefreshStatus)
import Vira.State.Acid qualified as Acid
import Vira.State.Core qualified as St
import Vira.State.Type
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Web.TablerIcons.Outline qualified as Icon

{- |
Status badge component with semantic color variants.

Displays status information with appropriate colors and styling.
Follows the Vira Design System status color guidelines.
Now uses the domain JobStatus type directly for type safety.

= Usage Examples

@
-- Job status badges
W.viraStatusBadge_ St.JobRunning
W.viraStatusBadge_ St.JobPending
W.viraStatusBadge_ (St.JobFinished St.JobSuccess)
W.viraStatusBadge_ (St.JobFinished St.JobFailure)
W.viraStatusBadge_ St.JobKilled
@

= Visual Design

Each status includes both semantic colors and appropriate icons:
- **Running**: Blue background with loader icon (spinner)
- **Pending**: Yellow background with clock icon
- **Success**: Green background with check icon
- **Failed**: Red background with X icon
- **Killed**: Darker red background with ban icon

= Type Safety

Uses 'St.JobStatus' directly from the domain model to prevent invalid status values.
-}

{- |
Get human-readable label for job status.

Provides consistent status text across the application.
-}
statusLabel :: St.JobStatus -> Text
statusLabel = \case
  St.JobRunning -> "Running"
  St.JobPending -> "Pending"
  St.JobFinished St.JobSuccess _ -> "Success"
  St.JobFinished St.JobFailure _ -> "Failed"
  St.JobFinished St.JobKilled _ -> "Killed"
  St.JobStale -> "Stale"

viraStatusBadge_ :: (Monad m) => St.JobStatus -> HtmlT m ()
viraStatusBadge_ jobStatus = do
  let label = statusLabel jobStatus
      (colorClass, iconSvg, iconClass) = case jobStatus of
        St.JobRunning -> ("bg-blue-100 dark:bg-blue-900/30 text-blue-800 dark:text-blue-300 border-blue-200 dark:border-blue-800", Icon.loader_2, "animate-spin")
        St.JobPending -> ("bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300 border-yellow-200 dark:border-yellow-800", Icon.clock, "")
        St.JobFinished St.JobSuccess _ -> ("bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300 border-green-200 dark:border-green-800", Icon.check, "")
        St.JobFinished St.JobFailure _ -> ("bg-red-100 dark:bg-red-900/30 text-red-800 dark:text-red-300 border-red-200 dark:border-red-800", Icon.x, "")
        St.JobFinished St.JobKilled _ -> ("bg-red-200 dark:bg-red-900/40 text-red-900 dark:text-red-200 border-red-300 dark:border-red-700", Icon.ban, "")
        St.JobStale -> ("bg-gray-200 dark:bg-gray-700 text-gray-800 dark:text-gray-300 border-gray-300 dark:border-gray-600", Icon.clock_off, "")
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $ do
    div_ [class_ $ "w-4 h-4 mr-2 flex items-center justify-center " <> iconClass] $ toHtmlRaw iconSvg
    toHtml label

viewAllJobStatus :: AppHtml ()
viewAllJobStatus = do
  -- Compute running jobs directly
  jobsData <- lift $ App.query Acid.GetRunningJobs
  let jobs = jobsData <&> \job -> (job.repo, job.jobId)
  div_ [class_ "flex items-center space-x-2", title_ "Build Status"] $ do
    indicator $ not $ null jobs
    forM_ jobs $ \(repo, jobId) -> do
      jobUrl <- lift $ getLinkUrl $ LinkTo.Job jobId
      a_ [href_ jobUrl] $ do
        span_ $ b_ $ toHtml $ unRepoName repo
        "/"
        span_ $ code_ $ toHtml @Text $ show jobId

indicator :: (Monad m) => Bool -> HtmlT m ()
indicator active = do
  let (iconSvg, classes) =
        if active
          then (Icon.loader_2, "text-green-500 dark:text-green-400 animate-spin")
          else (Icon.circle, "text-gray-500 dark:text-gray-400")
  div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
    toHtmlRaw iconSvg

{- |
Display refresh status for a repository.

Shows current refresh status with appropriate badge and timestamp information:
- **Never Refreshed**: Neutral indicator
- **Pending**: Yellow badge with queued time
- **In Progress**: Blue badge with spinner and start time
- **Success**: Green badge with completion time and duration
- **Failed**: Red badge with error message, completion time and duration
-}
data RefreshBadgeConfig = RefreshBadgeConfig
  { colorClasses :: Text
  , icon :: ByteString
  , iconExtraClasses :: Text
  , text :: Text
  , tooltip :: Maybe Text
  }

viraRefreshStatus_ :: RefreshState -> RepoName -> AppHtml ()
viraRefreshStatus_ refreshState repoName = do
  status <- liftIO $ getRefreshStatus refreshState repoName

  case status of
    NeverRefreshed ->
      span_ [class_ "text-xs text-gray-500 dark:text-gray-400"] "Never refreshed"
    Pending {queuedAt} -> do
      (timeAgo, _) <- formatTimestamp queuedAt
      refreshBadge
        RefreshBadgeConfig
          { colorClasses = "bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300 border-yellow-200 dark:border-yellow-800"
          , icon = Icon.clock
          , iconExtraClasses = ""
          , text = "Queued " <> timeAgo
          , tooltip = Nothing
          }
    InProgress {startedAt} -> do
      (timeAgo, _) <- formatTimestamp startedAt
      refreshBadge
        RefreshBadgeConfig
          { colorClasses = "bg-blue-100 dark:bg-blue-900/30 text-blue-800 dark:text-blue-300 border-blue-200 dark:border-blue-800"
          , icon = Icon.loader_2
          , iconExtraClasses = "animate-spin"
          , text = "Refreshing (started " <> timeAgo <> ")"
          , tooltip = Nothing
          }
    Success {completedAt, duration} -> do
      (timeAgo, _) <- formatTimestamp completedAt
      let durationText = formatDuration duration
      refreshBadge
        RefreshBadgeConfig
          { colorClasses = "bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300 border-green-200 dark:border-green-800"
          , icon = Icon.check
          , iconExtraClasses = ""
          , text = "Updated " <> timeAgo <> " (took " <> durationText <> ")"
          , tooltip = Nothing
          }
    Failed {completedAt, duration, errorMsg} -> do
      (timeAgo, _) <- formatTimestamp completedAt
      let durationText = formatDuration duration
      refreshBadge
        RefreshBadgeConfig
          { colorClasses = "bg-red-100 dark:bg-red-900/30 text-red-800 dark:text-red-300 border-red-200 dark:border-red-800"
          , icon = Icon.x
          , iconExtraClasses = ""
          , text = "Failed " <> timeAgo <> " (took " <> durationText <> ")"
          , tooltip = Just errorMsg
          }

-- Helper to render refresh status badge
refreshBadge :: (Monad m) => RefreshBadgeConfig -> HtmlT m ()
refreshBadge cfg =
  let attrs = [class_ $ "inline-flex items-center px-2 py-1 rounded text-xs font-medium border " <> cfg.colorClasses]
      attrsWithTooltip = maybe attrs (\t -> title_ t : attrs) cfg.tooltip
   in span_ attrsWithTooltip $ do
        div_ [class_ $ "w-3 h-3 mr-1.5 flex items-center justify-center " <> cfg.iconExtraClasses] $ toHtmlRaw cfg.icon
        toHtml cfg.text
