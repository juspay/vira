{-# LANGUAGE OverloadedRecordDot #-}

{- | Auto-build daemon that triggers CI jobs when branches update

Subscribes to branch update events and automatically enqueues builds,
with smart cancellation of pending jobs for skipped commits.
-}
module Vira.CI.AutoBuild (
  startAutoBuildDaemon,
) where

import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Time (addUTCTime, getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Git (Commit (..), RepoName)
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.CI.Client qualified as Client
import Vira.State.Acid (BranchUpdate (..), CancelPendingJobsInBranchA (..), SetRepoBranchesA (..))
import Prelude hiding (atomically)

{- | Start the auto-build daemon

Spawns a background worker that subscribes to branch update events
and automatically enqueues builds for new commits.
-}
startAutoBuildDaemon :: Eff AppStack ()
startAutoBuildDaemon = do
  log Info "🚀 Auto-build daemon starting..."
  void $ async autoBuildLoop
  log Info "🚀 Auto-build daemon started"

{- | Auto-build loop: subscribe to branch update events and enqueue builds

Subscribes to 'SetRepoBranchesA' events (branch updates from refresh daemon).
For each branch update, cancels pending jobs and enqueues a new build.
-}
autoBuildLoop :: Eff AppStack Void
autoBuildLoop = do
  tagCurrentThread "🚀"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    -- Use the event result (diff) returned by setRepoBranchesA
    whenJust (Events.matchUpdate @SetRepoBranchesA someUpdate) $ \(SetRepoBranchesA repo _branches, updates) ->
      handleBranchUpdates repo updates

-- | Handle branch updates by cancelling pending jobs and enqueueing new builds
handleBranchUpdates :: RepoName -> [BranchUpdate] -> Eff AppStack ()
handleBranchUpdates repo updates = do
  now <- liftIO getCurrentTime
  let oneHourAgo = addUTCTime (-3600) now
      recentUpdates = filter (\upd -> upd.newCommit.date >= oneHourAgo) updates
  forM_ recentUpdates $ \upd -> do
    void $ App.update $ CancelPendingJobsInBranchA repo upd.branch now
    Client.enqueueJob repo upd.branch upd.newCommit.id
