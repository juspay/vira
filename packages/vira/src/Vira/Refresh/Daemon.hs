{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Refresh.Daemon where

import Colog.Message (Message)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Data.Map qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
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
import Vira.Refresh.Type (RefreshConfig (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (Reader, ask, asks, readTVarIO)

-- | Run the refresh daemon in the background
runRefreshDaemon ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  TVar RefreshConfig ->
  Eff es ()
runRefreshDaemon configVar = do
  tagCurrentThread "🔄"
  log Info "Refresh daemon started"

  forever $ do
    config <- liftIO $ readTVarIO configVar
    if config.enabled
      then do
        log Debug "Starting refresh cycle"
        startTime <- liftIO getCurrentTime
        results <- refreshAllRepositories
        endTime <- liftIO getCurrentTime
        let duration = endTime `diffUTCTime` startTime
        log Info $ "Refresh completed in " <> show duration <> "s"
        log Debug $ "Refreshed " <> show (length results) <> " repositories"
      else do
        log Debug "Auto-refresh disabled, skipping cycle"

    -- Wait for next cycle
    let delayMicroseconds = round (config.refreshInterval * 1_000_000)
    liftIO $ threadDelay delayMicroseconds

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
