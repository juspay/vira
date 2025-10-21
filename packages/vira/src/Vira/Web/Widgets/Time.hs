-- | Time display components
module Vira.Web.Widgets.Time (
  viraUTCTime_,
  viraDuration_,
  viraRelativeTime_,
) where

import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Lucid
import Vira.Lib.TimeExtra (formatDuration, formatRelativeTime, formatTimestamp)
import Vira.Web.Lucid (AppHtml)

-- | UTC timestamp with local timezone display and tooltip
viraUTCTime_ :: UTCTime -> AppHtml ()
viraUTCTime_ utcTime = do
  (formatted, fullTimestamp) <- formatTimestamp utcTime
  span_
    [ class_ "text-sm text-gray-900 dark:text-gray-100 cursor-help"
    , title_ fullTimestamp
    ]
    $ toHtml formatted

-- | Duration badge component
viraDuration_ :: NominalDiffTime -> AppHtml ()
viraDuration_ duration = do
  span_ [class_ "text-xs text-gray-600 dark:text-gray-300 bg-gray-100 dark:bg-gray-700 px-2 py-1 rounded"] $
    toHtml $
      formatDuration duration

-- | Relative time display (e.g., "2min ago", "3hr ago") with full timestamp tooltip
viraRelativeTime_ :: UTCTime -> AppHtml ()
viraRelativeTime_ utcTime = do
  now <- liftIO getCurrentTime
  (_, fullTimestamp) <- formatTimestamp utcTime
  let relativeText = formatRelativeTime now utcTime
  span_
    [ class_ "text-sm text-gray-600 dark:text-gray-300 cursor-help"
    , title_ fullTimestamp
    ]
    $ toHtml relativeText
