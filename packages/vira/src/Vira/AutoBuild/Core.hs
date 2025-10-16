{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core auto-build logic
module Vira.AutoBuild.Core (
  -- * Build scheduling
  scheduleAutoBuild,
  getAutoBuildStatus,

  -- * Branch change detection
  detectBranchChanges,
) where

import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (Debug, Info), log)
import Effectful.Git (BranchName, Commit (..), CommitID, RepoName)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.Type (ViraRuntimeState (..))
import Vira.AutoBuild.Type (AutoBuildState (..), BuildPriority (..), BuildRequest (..))
import Prelude hiding (Reader, ask, asks)

-- | Detect branch changes and schedule builds for changed branches
detectBranchChanges ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  RepoName ->
  Map BranchName Commit ->
  Eff es ()
detectBranchChanges repo branches = do
  st <- asks @ViraRuntimeState (.autoBuildState)
  lastBuilt <- liftIO $ readTVarIO st.lastBuiltCommit

  -- Find branches with new commits
  let changedBranches =
        [ (branchName, commit)
        | (branchName, commit) <- Map.toList branches
        , let lastCommit = Map.lookup (repo, branchName) lastBuilt
        , lastCommit /= Just commit.id
        ]

  -- Schedule builds for changed branches
  forM_ changedBranches $ \(branchName, commit) -> do
    log Debug $ "Detected change on branch " <> show branchName <> ": " <> show commit.id
    scheduleAutoBuild repo branchName commit.id Auto

-- | Schedule a build with given priority
scheduleAutoBuild ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  RepoName ->
  BranchName ->
  CommitID ->
  BuildPriority ->
  Eff es ()
scheduleAutoBuild repo branch commit prio = do
  st <- asks @ViraRuntimeState (.autoBuildState)
  now <- liftIO getCurrentTime

  let request = BuildRequest {repo, branch, commit, priority = prio, queuedAt = now}
      key = (repo, branch)

  atomically $ modifyTVar' st.pendingBuilds $ \pending ->
    case Map.lookup key pending of
      Nothing ->
        -- No pending build, add this one
        Map.insert key request pending
      Just existing ->
        -- Already have a pending build for this branch
        -- Replace with latest commit (implements "skip intermediate commits" strategy)
        if prio >= existing.priority || commit > existing.commit
          then Map.insert key request pending
          else pending

  log Info $ "Queued auto-build for " <> show branch <> " at " <> show commit <> " (prio: " <> show prio <> ")"

-- | Get current auto-build status for debugging
getAutoBuildStatus ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es (Map (RepoName, BranchName) BuildRequest)
getAutoBuildStatus = do
  st <- asks @ViraRuntimeState (.autoBuildState)
  liftIO $ readTVarIO st.pendingBuilds
