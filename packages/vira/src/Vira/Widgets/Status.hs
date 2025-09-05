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
module Vira.Widgets.Status (
  viraStatusBadge_,
  viewAllJobStatus,
  indicator,
  statusLabel,
  statusDotClass,
  statusTextClass,
) where

import Lucid
import Vira.App.AcidState qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.State.Acid qualified as Acid
import Vira.State.Core qualified as St
import Vira.State.Type
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
  St.JobFinished St.JobSuccess -> "Success"
  St.JobFinished St.JobFailure -> "Failed"
  St.JobKilled -> "Killed"

{- |
Get Tailwind CSS classes for status dots.

Uses explicit class names instead of dynamic concatenation to ensure
Tailwind's CSS scanner can find and include these classes in the build.
-}
statusDotClass :: St.JobStatus -> Text
statusDotClass = \case
  St.JobRunning -> " bg-blue-500 animate-pulse"
  St.JobPending -> " bg-yellow-500"
  St.JobFinished St.JobSuccess -> " bg-green-500"
  St.JobFinished St.JobFailure -> " bg-red-500"
  St.JobKilled -> " bg-red-500"

{- |
Get Tailwind CSS classes for status text.

Uses explicit class names instead of dynamic concatenation to ensure
Tailwind's CSS scanner can find and include these classes in the build.
-}
statusTextClass :: St.JobStatus -> Text
statusTextClass = \case
  St.JobRunning -> " text-blue-600"
  St.JobPending -> " text-yellow-600"
  St.JobFinished St.JobSuccess -> " text-green-600"
  St.JobFinished St.JobFailure -> " text-red-600"
  St.JobKilled -> " text-red-600"

viraStatusBadge_ :: (Monad m) => St.JobStatus -> HtmlT m ()
viraStatusBadge_ jobStatus = do
  let label = statusLabel jobStatus
      (colorClass, iconSvg, iconClass) = case jobStatus of
        St.JobRunning -> ("bg-blue-100 text-blue-800 border-blue-200", Icon.loader_2, "animate-spin")
        St.JobPending -> ("bg-yellow-100 text-yellow-800 border-yellow-200", Icon.clock, "")
        St.JobFinished St.JobSuccess -> ("bg-green-100 text-green-800 border-green-200", Icon.check, "")
        St.JobFinished St.JobFailure -> ("bg-red-100 text-red-800 border-red-200", Icon.x, "")
        St.JobKilled -> ("bg-red-200 text-red-900 border-red-300", Icon.ban, "")
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $ do
    div_ [class_ $ "w-4 h-4 mr-2 flex items-center justify-center " <> iconClass] $ toHtmlRaw iconSvg
    toHtml label

viewAllJobStatus :: AppHtml ()
viewAllJobStatus = do
  -- Compute running jobs directly
  jobsData <- lift $ App.query Acid.GetRunningJobs
  let jobs = jobsData <&> \job -> (job.jobRepo, job.jobId)
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
          then (Icon.loader_2, "text-green-500 animate-spin")
          else (Icon.circle, "text-gray-500")
  div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
    toHtmlRaw iconSvg
