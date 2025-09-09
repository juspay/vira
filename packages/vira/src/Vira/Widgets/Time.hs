{- |
Vira Design System - Time Display Components

This module contains time display components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Time Display Components
- 'viraUTCTime_' - UTC timestamp with local timezone display and full precision tooltip

= Usage Guidelines

Use these components for displaying timestamps, durations, and time-related information.
Always prefer these components over raw HTML to maintain design consistency.

= Design Principles

- Local Timezone Display: Shows user-friendly local time
- Full Precision Tooltips: Complete UTC information on hover
- Accessibility: Proper cursor hints and semantic markup
- Consistency: Standardized time formatting across the application
-}
module Vira.Widgets.Time (
  viraUTCTime_,
) where

import Data.Time (UTCTime)
import Lucid
import Vira.App.Lucid (AppHtml)
import Vira.Lib.TimeExtra (formatTimestamp)

{- |
UTC timestamp display component with local timezone conversion.

Displays a UTC timestamp in user-friendly local time format with seconds precision.
Shows the full UTC timestamp with complete precision in a tooltip on hover.

Example usage:
@
viraUTCTime_ someTimestamp
@

This renders as local time like "2025-09-09 14:30:45" with a tooltip showing
the full UTC timestamp "2025-09-09 21:30:45.123456789 UTC".
-}
viraUTCTime_ :: UTCTime -> AppHtml ()
viraUTCTime_ utcTime = do
  (formatted, fullTimestamp) <- formatTimestamp utcTime
  span_
    [ class_ "text-sm text-gray-900 cursor-help"
    , title_ fullTimestamp
    ]
    $ toHtml formatted
