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
) where

import Lucid
import Vira.State.Core qualified as St

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

= Color Guidelines

Colors are automatically applied based on job status:
- Green: Successful jobs
- Red: Failed or killed jobs
- Yellow: Pending jobs
- Blue: Running jobs

= Type Safety

Uses 'St.JobStatus' directly from the domain model to prevent invalid status values.
-}
viraStatusBadge_ :: St.JobStatus -> Html ()
viraStatusBadge_ jobStatus = do
  let (statusText, colorClass) = case jobStatus of
        St.JobRunning -> ("Running" :: Text, "bg-blue-100 text-blue-800 border-blue-200")
        St.JobPending -> ("Pending" :: Text, "bg-yellow-100 text-yellow-800 border-yellow-200")
        St.JobFinished St.JobSuccess -> ("Success" :: Text, "bg-green-100 text-green-800 border-green-200")
        St.JobFinished St.JobFailure -> ("Failed" :: Text, "bg-red-100 text-red-800 border-red-200")
        St.JobKilled -> ("Killed" :: Text, "bg-red-200 text-red-900 border-red-300")
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $
    toHtml statusText
