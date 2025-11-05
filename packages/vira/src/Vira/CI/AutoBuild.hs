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
import Vira.CI.AutoBuild.Type (AutoBuildNewBranches (..))
import Vira.CI.Client qualified as Client
import Vira.State.Acid (BranchUpdate (..), CancelPendingJobsInBranchA (..), SetRepoBranchesA (..))
import Prelude hiding (atomically)

{- | Start the auto-build daemon

Spawns a background worker that subscribes to branch update events
and automatically enqueues builds for new commits.
-}
startAutoBuildDaemon :: AutoBuildNewBranches -> Eff AppStack ()
startAutoBuildDaemon autoBuildNewBranches = do
  log Info "🚀 Auto-build daemon starting..."
  void $ async (autoBuildLoop autoBuildNewBranches)
  log Info "🚀 Auto-build daemon started"

{- | Auto-build loop: subscribe to branch update events and enqueue builds

Subscribes to 'SetRepoBranchesA' events (branch updates from refresh daemon).
For each branch update, cancels pending jobs and enqueues a new build.
-}
autoBuildLoop :: AutoBuildNewBranches -> Eff AppStack Void
autoBuildLoop autoBuildNewBranches = do
  tagCurrentThread "🚀"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    -- Use the event result (diff) returned by setRepoBranchesA
    whenJust (Events.matchUpdate @SetRepoBranchesA someUpdate) $ \(SetRepoBranchesA repo _branches, updates) ->
      handleBranchUpdates autoBuildNewBranches repo updates

-- | Handle branch updates by cancelling pending jobs and enqueueing new builds
handleBranchUpdates :: AutoBuildNewBranches -> RepoName -> [BranchUpdate] -> Eff AppStack ()
handleBranchUpdates autoBuildNewBranches repo updates = do
  now <- liftIO getCurrentTime
  let oneHourAgo = addUTCTime (-3600) now
      newBranch upd = not upd.wasPreviouslyBuilt
      shouldSkipBranch upd =
        {- HLINT ignore "Use &&" -}
        and
          [ -- Ignore old branches
            upd.newCommit.date < oneHourAgo
          , -- Build only already-built branches, unless autoBuildNewBranches is enabled
            not $ coerce autoBuildNewBranches || not (newBranch upd)
          ]
  forM_ updates $ \upd -> do
    let isOld = upd.newCommit.date < oneHourAgo
        isNew = newBranch upd
        reason
          | isOld && isNew = "old commit + new branch"
          | isOld = "old commit"
          | isNew = "new branch (autoBuildNewBranches=False)"
          | otherwise = "unknown"
    if shouldSkipBranch upd
      then
        log Info $
          "⏭️  Skipping auto-build for "
            <> toText repo
            <> "/"
            <> toText upd.branch
            <> " at "
            <> toText upd.newCommit.id
            <> " ("
            <> reason
            <> ")"
      else do
        log Info $
          "🔨 Enqueueing auto-build for "
            <> toText repo
            <> "/"
            <> toText upd.branch
            <> " at "
            <> toText upd.newCommit.id
            <> if isNew then " (new branch)" else ""
        void $ App.update $ CancelPendingJobsInBranchA repo upd.branch now
        Client.enqueueJob repo upd.branch upd.newCommit.id
