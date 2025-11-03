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
  viraSmartRefreshButton_,
  viewAllJobStatus,
  indicator,
  statusLabel,
) where

import Data.Time (getCurrentTime)
import Effectful.Git (RepoName (..))
import Lucid
import Vira.App.AcidState qualified as App
import Vira.Lib.TimeExtra (formatDuration, formatRelativeTime, formatTimestamp)
import Vira.Refresh (getRepoRefreshStatus)
import Vira.Refresh.Type (RefreshOutcome (..), RefreshResult (..), RefreshStatus (..))
import Vira.State.Acid qualified as Acid
import Vira.State.Core qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl)
import Vira.Web.Widgets.Button qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (asks)

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
  St.JobPending -> "Queued"
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
  -- Compute running and queued jobs count
  runningJobs <- lift $ App.query Acid.GetRunningJobsA
  queuedJobs <- lift $ App.query Acid.GetPendingJobsA
  let runningCount = length runningJobs
      queuedCount = length queuedJobs
      active = runningCount > 0 || queuedCount > 0
  indexUrl <- lift $ getLinkUrl LinkTo.Home
  a_ [href_ indexUrl, class_ "flex items-center space-x-2 text-white hover:bg-white/20 px-3 py-1 rounded-lg transition-colors", title_ "View all jobs"] $ do
    indicator active
    span_ [class_ "text-sm font-medium"] $
      if active
        then toHtml $ show @Text runningCount <> " running, " <> show @Text queuedCount <> " queued"
        else "No builds running"

indicator :: (Monad m) => Bool -> HtmlT m ()
indicator active = do
  let (iconSvg, classes) =
        if active
          then (Icon.loader_2, "text-green-500 dark:text-green-400 animate-spin")
          else (Icon.circle, "text-gray-500 dark:text-gray-400")
  div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
    toHtmlRaw iconSvg

{- |
Smart refresh button that adapts appearance based on refresh status.

Combines status display and refresh action into a single button that:
- Shows current refresh state in the label
- Changes color based on status (blue when refreshing, green after success, red after failure)
- Automatically disables during Pending/InProgress states
- Displays appropriate icons (spinner, check, alert)

= Usage Example

@
Status.viraSmartRefreshButton_ repo.name
@

= Visual States

- **Never Refreshed**: Gray secondary button "Refresh Branches"
- **Pending/In Progress**: Blue button with spinner "Refreshing..." (disabled)
- **Success**: Secondary button with subtle green tint "Refresh (updated 5m ago)"
- **Failed**: Secondary button with red tint "Retry Refresh (failed 2m ago)"
-}
viraSmartRefreshButton_ :: RepoName -> AppHtml ()
viraSmartRefreshButton_ repo = do
  updateLink <- lift $ getLink $ LinkTo.RepoUpdate repo
  status <- lift $ getRepoRefreshStatus repo
  now <- liftIO getCurrentTime

  case status of
    NeverRefreshed ->
      W.viraRequestButton_
        W.ButtonSecondary
        updateLink
        [title_ "Refresh branches from remote"]
        $ do
          W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
          "Refresh Branches"
    Pending {queuedAt} -> do
      (fullTime, _) <- formatTimestamp queuedAt
      button_
        [ class_ "inline-flex items-center px-4 py-2 rounded-lg text-sm font-medium border transition-colors bg-blue-100 dark:bg-blue-900/30 text-blue-800 dark:text-blue-300 border-blue-200 dark:border-blue-800 cursor-not-allowed"
        , disabled_ "disabled"
        , title_ $ "Queued at " <> fullTime
        ]
        $ do
          div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center animate-spin"] $ toHtmlRaw Icon.loader_2
          "Queued..."
    InProgress {startedAt} -> do
      (fullTime, _) <- formatTimestamp startedAt
      button_
        [ class_ "inline-flex items-center px-4 py-2 rounded-lg text-sm font-medium border transition-colors bg-blue-100 dark:bg-blue-900/30 text-blue-800 dark:text-blue-300 border-blue-200 dark:border-blue-800 cursor-not-allowed"
        , disabled_ "disabled"
        , title_ $ "Started at " <> fullTime
        ]
        $ do
          div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center animate-spin"] $ toHtmlRaw Icon.loader_2
          "Refreshing..."
    Completed result ->
      renderCompletedButton now updateLink result
  where
    renderCompletedButton now updateLink RefreshResult {completedAt, duration, outcome} = do
      (fullTime, _) <- formatTimestamp completedAt
      let relativeTime = formatRelativeTime now completedAt
          durationText = formatDuration duration
      case outcome of
        Success ->
          W.viraRequestButton_
            W.ButtonSecondary
            updateLink
            [title_ $ "Last refresh: " <> fullTime <> " (took " <> durationText <> ")"]
            $ do
              W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
              toHtml $ "Refresh (" <> relativeTime <> ")"
        Failure errorMsg ->
          W.viraRequestButton_
            W.ButtonSecondary
            updateLink
            [ title_ $ errorMsg <> " (failed at " <> fullTime <> ", took " <> durationText <> ")"
            , class_ "!bg-red-50 dark:!bg-red-900/20 !text-red-700 dark:!text-red-300 !border-red-200 dark:!border-red-800 hover:!bg-red-100 dark:hover:!bg-red-900/30"
            ]
            $ do
              div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.alert_triangle
              toHtml $ "Retry (" <> relativeTime <> ")"
