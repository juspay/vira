{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Refresh.Daemon where

import Colog.Message (Message)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTQueue, readTVarIO, writeTQueue)
import Data.Map qualified as Map
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
import Vira.Refresh.Type (RefreshCommand (..), RefreshConfig (..), RefreshStatus (..))
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

  -- Main command processing loop
  forever $ do
    queue <- asks (.refreshQueue)
    command <- liftIO $ atomically $ readTQueue queue

    case command of
      RefreshRepo repoName priority -> do
        log Info $ "Processing " <> show priority <> " refresh for " <> repoName.unRepoName
        processRefreshCommand command
      RefreshAll -> do
        log Debug "Processing periodic refresh for all repositories"
        processRefreshCommand command

-- | Run the periodic timer that enqueues RefreshAll commands
runPeriodicTimer ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  ) =>
  Eff es ()
runPeriodicTimer = forever $ do
  config <- asks (.refreshConfig) >>= liftIO . readTVarIO
  if config.enabled
    then do
      queue <- asks (.refreshQueue)
      liftIO $ atomically $ writeTQueue queue RefreshAll
      log Debug "Enqueued periodic refresh"
    else do
      log Debug "Auto-refresh disabled, skipping timer"

  -- Wait for next cycle
  let delayMicroseconds = round (config.refreshInterval * 1_000_000)
  liftIO $ threadDelay delayMicroseconds

-- | Process a refresh command
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
      -- Update refresh metadata with failure
      currentTime <- liftIO getCurrentTime
      _ <- App.update $ St.UpdateRepoRefreshA repo.name currentTime (RefreshFailure errorMsg)
      pure $ Left errorMsg
    Right branches -> do
      -- Update state with new branches
      _ <- App.update $ St.SetRepoBranchesA repo.name branches
      -- Update refresh metadata with success
      currentTime <- liftIO getCurrentTime
      _ <- App.update $ St.UpdateRepoRefreshA repo.name currentTime RefreshSuccess
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
