-- | Time display components
module Vira.Web.Widgets.Time (
  viraUTCTime_,
  viraDuration_,
  viraRelativeTime_,
  viraUptime_,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Lucid
import Vira.Lib.TimeExtra (formatDuration, formatRelativeTime, formatTimestamp)
import Vira.Web.Lucid (AppHtml)

-- | 'UTCTime' timestamp with local timezone display and tooltip
viraUTCTime_ :: UTCTime -> AppHtml ()
viraUTCTime_ utcTime = do
  (formatted, fullTimestamp) <- formatTimestamp utcTime
  span_
    [ class_ "text-sm text-gray-900 dark:text-gray-100 cursor-help"
    , title_ fullTimestamp
    ]
    $ toHtml formatted

-- | Duration badge component for 'NominalDiffTime'
viraDuration_ :: NominalDiffTime -> AppHtml ()
viraDuration_ duration = do
  span_ [class_ "text-xs text-gray-600 dark:text-gray-300 bg-gray-100 dark:bg-gray-700 px-2 py-1 rounded"] $
    toHtml $
      formatDuration duration

-- | Relative time display (e.g., @2min ago@, @3hr ago@) with full timestamp tooltip
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

{- | Real-time uptime display that updates every second

Shows server uptime in a compact format (e.g., @Uptime: 2h 15m 30s@).
Self-contained with inline JavaScript.
-}
viraUptime_ :: UTCTime -> AppHtml ()
viraUptime_ startTime = do
  now <- liftIO getCurrentTime
  let uptime = diffUTCTime now startTime
      startTimestamp = show @Text (floor (realToFrac (utcTimeToPOSIXSeconds startTime) :: Double) :: Integer)
  span_
    [ title_ "Server uptime"
    , id_ "uptime"
    ]
    $ toHtml
    $ "Uptime: " <> formatDuration uptime
  script_ $
    unlines
      [ "(function() {"
      , "  const el = document.getElementById('uptime');"
      , "  const start = " <> startTimestamp <> ";"
      , "  function update() {"
      , "    const s = Math.floor(Date.now() / 1000 - start);"
      , "    const h = Math.floor(s / 3600);"
      , "    const m = Math.floor((s % 3600) / 60);"
      , "    const sec = s % 60;"
      , "    if (h === 0 && m === 0) el.textContent = 'Uptime: ' + sec + 's';"
      , "    else if (h === 0) el.textContent = 'Uptime: ' + m + 'm ' + sec + 's';"
      , "    else el.textContent = 'Uptime: ' + h + 'h ' + m + 'm ' + sec + 's';"
      , "  }"
      , "  setInterval(update, 1000);"
      , "})();"
      ]
