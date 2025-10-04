-- | Time display components
module Vira.Widgets.Time (
  viraUTCTime_,
  viraDuration_,
) where

import Data.Time (NominalDiffTime, UTCTime)
import Lucid
import Vira.App.Lucid (AppHtml)
import Vira.Lib.TimeExtra (formatDuration, formatTimestamp)

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
