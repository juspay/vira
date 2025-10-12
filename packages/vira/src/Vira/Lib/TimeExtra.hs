{- |
Time formatting utilities.
-}
module Vira.Lib.TimeExtra (
  formatDuration,
  formatRelativeTime,
  formatTimestamp,
) where

import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, utcToLocalTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.LocalTime (getTimeZone)

{- |
Format duration for display (e.g., "2m 34s", "1h 15m 30s").

Converts a NominalDiffTime to a human-readable duration string with appropriate units.
-}
formatDuration :: NominalDiffTime -> Text
formatDuration diffTime =
  let totalSeconds = floor $ nominalDiffTimeToSeconds diffTime :: Int
      hours = totalSeconds `div` 3600
      minutes = (totalSeconds `mod` 3600) `div` 60
      seconds = totalSeconds `mod` 60
   in case (hours, minutes, seconds) of
        (0, 0, s) -> show s <> "s"
        (0, m, s) -> show m <> "m " <> show s <> "s"
        (h, m, s) -> show h <> "h " <> show m <> "m " <> show s <> "s"

{- |
Format relative time for display (e.g., "2min ago", "3hr ago", "just now").

Converts the difference between two UTCTime values to a human-readable relative time string.
For differences greater than a week, shows the absolute date instead.
Uses concise abbreviations for space-constrained UIs.
-}
formatRelativeTime :: UTCTime -> UTCTime -> Text
formatRelativeTime now commitTime =
  let diffSeconds = round $ diffUTCTime now commitTime :: Integer
      minutes = diffSeconds `div` 60
      hours = minutes `div` 60
      days = hours `div` 24
   in if diffSeconds < 60
        then "just now"
        else
          if minutes < 60
            then toText (show @Text minutes) <> "min ago"
            else
              if hours < 24
                then toText (show @Text hours) <> "hr ago"
                else
                  if days < 7
                    then toText (show @Text days) <> "d ago"
                    else toText $ formatTime defaultTimeLocale "%b %d, %Y" commitTime

{- |
Format a UTC timestamp for display with local timezone conversion.

Returns a tuple of (formatted display text, full UTC timestamp string).
The formatted text shows seconds precision in local time, while the full timestamp
provides complete UTC information suitable for tooltips.

Example: ("2025-09-09 14:30:45", "2025-09-09 21:30:45.123456789 UTC")
-}
formatTimestamp :: (MonadIO m) => UTCTime -> m (Text, Text)
formatTimestamp utcTime = do
  tz <- liftIO $ getTimeZone utcTime
  let localTime = utcToLocalTime tz utcTime
      formatted = toText $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
      fullTimestamp = show @Text utcTime
  pure (formatted, fullTimestamp)
