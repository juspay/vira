{- | Cleanup daemon for automatic deletion of old jobs

This module provides a background daemon that periodically removes old jobs
from both acid-state and the filesystem. It runs hourly and can be disabled
by setting retention to 0 days.
-}
module Vira.CI.Cleanup.Daemon (
  startCleanupDaemon,
) where

import Data.Time (addUTCTime, getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (async)
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.CI.Cleanup (deleteOldJob)
import Vira.State.Acid qualified as St

{- | Start the cleanup daemon

If retentionDays is 0, the daemon is disabled and logs a message.
Otherwise, spawns a background thread that runs hourly to clean up old jobs.
-}
startCleanupDaemon :: Natural -> Eff AppStack ()
startCleanupDaemon retentionDays = do
  if retentionDays == 0
    then log Info "🧹 Cleanup daemon disabled (retention: 0 days)"
    else do
      void $ async $ cleanupLoop retentionDays
      log Info $
        "🧹 Cleanup daemon started (checking every hour, retention: "
          <> show retentionDays
          <> " days)"

{- | Cleanup loop: runs hourly to remove old jobs

Queries for jobs older than the retention period and deletes them.
If retentionDays is 0, skips cleanup (though this shouldn't be reached
given the check in startCleanupDaemon).
-}
cleanupLoop :: Natural -> Eff AppStack Void
cleanupLoop retentionDays = do
  tagCurrentThread "🧹"
  infinitely $ do
    when (retentionDays > 0) $ do
      now <- liftIO getCurrentTime
      let cutoffTime = addUTCTime (negate $ fromIntegral retentionDays * 86400) now
      oldJobs <- App.query $ St.GetOldJobsA cutoffTime

      unless (null oldJobs) $ do
        log Info $
          "Found "
            <> show (length oldJobs)
            <> " jobs older than "
            <> show retentionDays
            <> " days"
        forM_ oldJobs $ \(jobId, endTime, workDir) -> deleteOldJob now jobId endTime workDir

    threadDelay (60 * 60 * 1000000) -- 1 hour in microseconds
