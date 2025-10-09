{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Refresh.Daemon where

import Colog.Message (Message)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO, retry)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, async)
import Effectful.Error.Static (
  Error,
  runErrorNoCallStack,
 )
import Effectful.Git (BranchName, RepoName)
import Effectful.Git qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Process (Process)
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App qualified as App
import Vira.App.Stack (AppState (..))
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.Logging (Severity (..), log, tagCurrentThread)
import Vira.Refresh.Type (RefreshCommand (..), RefreshConfig (..), RefreshDaemon (..), RefreshPriority (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (Reader, ask, asks, readTVarIO)

-- | Run the refresh daemon in the background
runRefreshDaemon ::
  ( Log Message :> es
  , Reader AppState :> es
  , Concurrent :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  Eff es ()
runRefreshDaemon = do
  tagCurrentThread "🔄"
  log Info "Refresh daemon started"

  -- Start periodic timer in background
  _ <- async runPeriodicTimer

  -- Main refresh processing loop
  forever $ do
    refreshDaemonTVar <- asks (.refreshDaemon)
    mdaemon <- liftIO $ readTVarIO refreshDaemonTVar
    case mdaemon of
      Just daemon -> do
        let pendingSet = daemon.state.pendingRepos

        -- Wait for a repo that needs refresh
        repoName <- liftIO $ Control.Concurrent.STM.atomically $ do
          currentSet <- readTVar pendingSet
          case Set.minView currentSet of
            Just (name, rest) -> do
              writeTVar pendingSet rest -- Remove from pending set
              pure name
            Nothing -> retry -- Wait for repos to be added

        -- Set status to pending (in case it wasn't already)
        let statusMap = daemon.state.statuses
        now <- liftIO getCurrentTime
        liftIO $
          Control.Concurrent.STM.atomically $
            modifyTVar statusMap $
              Map.insert repoName (RefreshPending Automatic now)

        -- Process the refresh
        repos <- App.query St.GetAllReposA
        case find (\r -> r.name == repoName) repos of
          Just repo -> do
            void $ refreshRepository repo
          Nothing -> do
            log Warning $ "Repository not found: " <> repoName.unRepoName
      Nothing -> do
        log Debug "Refresh daemon not yet initialized, waiting..."
        liftIO $ threadDelay 100_000 -- Wait 100ms before checking again

-- | Run the periodic timer that adds all repos to the refresh set
runPeriodicTimer ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  ) =>
  Eff es ()
runPeriodicTimer = forever $ do
  refreshDaemonTVar <- asks (.refreshDaemon)
  mdaemon <- liftIO $ readTVarIO refreshDaemonTVar
  case mdaemon of
    Just daemon -> do
      config <- liftIO $ readTVarIO daemon.state.config
      if config.enabled
        then do
          -- Get all repos and add them to pending set
          allRepos <- App.query St.GetAllReposA
          let allNames = Set.fromList $ map (.name) allRepos

          let pendingSet = daemon.state.pendingRepos
          liftIO $ Control.Concurrent.STM.atomically $ modifyTVar pendingSet (Set.union allNames)

          log Debug $ "Added " <> show (length allNames) <> " repos to refresh queue"
        else do
          log Debug "Auto-refresh disabled, skipping timer"

      -- Wait for next cycle
      let delayMicroseconds = round (config.refreshInterval * 1_000_000)
      liftIO $ threadDelay delayMicroseconds
    Nothing -> do
      log Debug "Refresh daemon not yet initialized in timer, waiting..."
      liftIO $ threadDelay 100_000 -- Wait 100ms before checking again

-- \| Process a refresh command
processRefreshCommand ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  RefreshCommand ->
  Eff es ()
processRefreshCommand command = do
  startTime <- liftIO getCurrentTime

  results <- case command of
    RefreshRepo repoName _ -> do
      -- Refresh single repository
      repos <- App.query St.GetAllReposA
      case find (\r -> r.name == repoName) repos of
        Just repo -> do
          result <- refreshRepository repo
          pure [(repoName, result)]
        Nothing -> do
          log Warning $ "Repository not found: " <> repoName.unRepoName
          pure []
    RefreshAll -> do
      -- Refresh all repositories
      refreshAllRepositories

  endTime <- liftIO getCurrentTime
  let duration = endTime `diffUTCTime` startTime
  log Info $ "Refresh completed in " <> show duration <> "s"
  log Debug $ "Processed " <> show (length results) <> " repositories"

-- | Refresh a single repository
refreshRepository ::
  ( HasCallStack
  , Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  Repo ->
  Eff es (Either Text (Map BranchName Git.Commit))
refreshRepository repo = do
  supervisor <- asks (.supervisor)
  let mirrorPath = Workspace.mirrorPath supervisor repo.name

  log Debug $ "Refreshing repository: " <> repo.name.unRepoName

  result <- runErrorNoCallStack @Text $ do
    -- Sync the mirror
    Mirror.syncMirror repo.cloneUrl mirrorPath
    -- Get remote branches
    Git.remoteBranchesFromClone mirrorPath

  case result of
    Left errorMsg -> do
      log Warning errorMsg
      -- Update refresh status with failure
      currentTime <- liftIO getCurrentTime
      refreshDaemonTVar <- asks (.refreshDaemon)
      mdaemon <- liftIO $ readTVarIO refreshDaemonTVar
      case mdaemon of
        Just daemon -> do
          let statusMap = daemon.state.statuses
          liftIO $
            Control.Concurrent.STM.atomically $
              modifyTVar statusMap $
                Map.insert repo.name (RefreshFailure errorMsg currentTime)
        Nothing -> log Error "Refresh daemon not initialized for failure status update"
      pure $ Left errorMsg
    Right branches -> do
      -- Update state with new branches
      _ <- App.update $ St.SetRepoBranchesA repo.name branches
      -- Update refresh status with success
      currentTime <- liftIO getCurrentTime
      refreshDaemonTVar <- asks (.refreshDaemon)
      mdaemon <- liftIO $ readTVarIO refreshDaemonTVar
      case mdaemon of
        Just daemon -> do
          let statusMap = daemon.state.statuses
          liftIO $
            Control.Concurrent.STM.atomically $
              modifyTVar statusMap $
                Map.insert repo.name (RefreshSuccess currentTime)
        Nothing -> log Error "Refresh daemon not initialized for success status update"
      log Debug $ "Successfully refreshed " <> repo.name.unRepoName <> " (" <> show (Map.size branches) <> " branches)"
      pure $ Right branches

-- | Refresh all repositories
refreshAllRepositories ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  Eff es [(RepoName, Either Text (Map BranchName Git.Commit))]
refreshAllRepositories = do
  repos <- App.query St.GetAllReposA
  log Debug $ "Found " <> show (length repos) <> " repositories to refresh"

  -- Refresh each repository
  results <- forM repos $ \repo -> do
    result <- refreshRepository repo
    pure (repo.name, result)

  -- Log summary
  let successCount = length $ filter (isRight . snd) results
      failureCount = length $ filter (isLeft . snd) results
  log Info $ "Refresh cycle complete: " <> show successCount <> " successful, " <> show failureCount <> " failed"

  pure results
