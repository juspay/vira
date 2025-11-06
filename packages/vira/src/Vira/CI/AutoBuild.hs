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
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
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

-- | Represents the decision for a branch update
data BuildDecision
  = BuildBranch
  | SkipBranch SkipReason
  deriving stock (Show, Eq)

-- | Reason why a branch update was skipped
data SkipReason
  = -- | The commit is >1 hour old
    OldCommit
  | -- | This branch was never built before (maybe new) and autoBuildNewBranches is disabled
    NeverBuilt
  deriving stock (Show, Eq)

-- | Decide whether to build a branch update (pure, testable)
decideBuildAction ::
  AutoBuildNewBranches ->
  -- | Current time
  UTCTime ->
  -- | The update
  BranchUpdate ->
  BuildDecision
decideBuildAction (AutoBuildNewBranches autoBuildNewBranches) now upd =
  if
    | isNew && not autoBuildNewBranches -> SkipBranch NeverBuilt
    | isOld -> SkipBranch OldCommit
    | otherwise -> BuildBranch
  where
    oneHourAgo = addUTCTime (-3600) now
    isOld = upd.newCommit.date < oneHourAgo
    isNew = upd.neverBuilt

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
  forM_ updates $ \upd -> do
    case decideBuildAction autoBuildNewBranches now upd of
      SkipBranch reason ->
        log Info $
          "⏭️  Skipping auto-build for "
            <> toText repo
            <> "/"
            <> toText upd.branch
            <> " at "
            <> toText upd.newCommit.id
            <> " ("
            <> skipReasonText reason
            <> ")"
      BuildBranch -> do
        log Info $
          "🔨 Enqueueing auto-build for "
            <> toText repo
            <> "/"
            <> toText upd.branch
            <> " at "
            <> toText upd.newCommit.id
            <> if upd.neverBuilt then " (new branch)" else ""
        void $ App.update $ CancelPendingJobsInBranchA repo upd.branch now
        Client.enqueueJob repo upd.branch upd.newCommit.id
  where
    skipReasonText :: SkipReason -> Text
    skipReasonText = \case
      OldCommit -> "old commit"
      NeverBuilt -> "never build before (autoBuildNewBranches=False)"
