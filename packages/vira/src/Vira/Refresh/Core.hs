{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core refresh logic for Git repositories
module Vira.Refresh.Core (
  -- * Refresh operations
  refreshRepo,
  getPendingRepos,
  setStatusPending,
) where

import Colog (Message)
import Data.Acid (AcidState)
import Data.Acid qualified as Acid
import Data.Map.Strict qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (FileSystem)
import Effectful.Git (RepoName)
import Effectful.Git qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Process (Process)
import Vira.CI.Workspace qualified as Workspace
import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..), ViraState)
import Vira.Supervisor.Type (TaskSupervisor)

-- * Core refresh logic

-- | Refresh a single repository
refreshRepo ::
  ( IOE :> es
  , Log Message :> es
  , Process :> es
  , FileSystem :> es
  ) =>
  RefreshState ->
  TaskSupervisor ->
  AcidState ViraState ->
  Repo ->
  Eff es (Either Text ())
refreshRepo st supervisor acid repo = do
  -- Mark as InProgress
  startTime <- liftIO getCurrentTime
  liftIO $ atomically $ modifyTVar' st.statusMap $ Map.insert repo.name (InProgress startTime)

  -- Run the actual refresh
  let mirrorPath = Workspace.mirrorPath supervisor repo.name
  result <- runErrorNoCallStack @Text $ do
    Mirror.syncMirror repo.cloneUrl mirrorPath
    allBranches <- Git.remoteBranchesFromClone mirrorPath
    liftIO $ Acid.update acid $ St.SetRepoBranchesA repo.name allBranches

  -- Update status based on result
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  liftIO $
    atomically $
      modifyTVar' st.statusMap $
        Map.insert repo.name $
          case result of
            Left err -> Failed endTime duration err
            Right () -> Success endTime duration

  pure result

-- * Status updates

-- | Get all repos with Pending status, sorted by priority (Now first)
getPendingRepos :: RefreshState -> IO [(RepoName, RefreshPriority)]
getPendingRepos st = do
  statusMap <- readTVarIO st.statusMap
  let pending = [(repo, prio) | (repo, Pending _ prio) <- Map.toList statusMap]
  pure $ sortWith (Down . snd) pending -- Now > Normal

-- | Set a repository's status to Pending
setStatusPending :: RefreshState -> RepoName -> RefreshPriority -> IO ()
setStatusPending st repo prio = do
  now <- getCurrentTime
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Pending now prio)
