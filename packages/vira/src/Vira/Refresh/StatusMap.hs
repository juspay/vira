{-# LANGUAGE OverloadedRecordDot #-}

-- | Low-level statusMap manipulation functions
module Vira.Refresh.StatusMap (
  markRepoCompleted,
  markRepoPending,
  removeRepoFromRefreshState,
  initializeRefreshState,
  popNextPendingRepo,
) where

import Control.Concurrent.STM (retry)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Concurrent.STM (atomically)
import Effectful.Git (RepoName)
import Vira.Refresh.Type (RefreshPriority, RefreshResult, RefreshStatus (..))
import Vira.State.Type (Repo (..))
import Prelude hiding (atomically)

{- | Initialize refresh state from persisted repo data

Loads all repos' lastRefresh status into the TVar map. Called on daemon startup.
-}
initializeRefreshState :: (Concurrent :> es) => TVar (Map RepoName RefreshStatus) -> [Repo] -> Eff es ()
initializeRefreshState statusMap repos = do
  let initialStatus =
        Map.fromList
          [ (repo.name, Completed result)
          | repo <- repos
          , Just result <- [repo.lastRefresh]
          ]
  atomically $ writeTVar statusMap initialStatus

-- | Mark a repository as Pending with given priority
markRepoPending :: (Concurrent :> es) => TVar (Map RepoName RefreshStatus) -> RepoName -> UTCTime -> RefreshPriority -> Eff es ()
markRepoPending statusMap repo now prio = do
  atomically $ modifyTVar' statusMap $ Map.insert repo (Pending now prio)

-- | Mark a repository as Completed
markRepoCompleted :: (Concurrent :> es) => TVar (Map RepoName RefreshStatus) -> RepoName -> RefreshResult -> Eff es ()
markRepoCompleted statusMap repo result = do
  atomically $ modifyTVar' statusMap $ Map.insert repo (Completed result)

-- | Remove a repository from refresh state (cleanup on deletion)
removeRepoFromRefreshState :: (Concurrent :> es) => TVar (Map RepoName RefreshStatus) -> RepoName -> Eff es ()
removeRepoFromRefreshState statusMap repo = do
  atomically $ modifyTVar' statusMap (Map.delete repo)

{- | Atomically pop the next pending repo and mark it as InProgress
Blocks (via STM retry) when no pending repos available
-}
popNextPendingRepo :: (Concurrent :> es, IOE :> es) => TVar (Map RepoName RefreshStatus) -> Eff es RepoName
popNextPendingRepo statusMap = do
  now <- liftIO getCurrentTime
  atomically $ do
    sm <- readTVar statusMap
    -- Find all pending repos sorted by priority
    let pending = [(repo, prio) | (repo, Pending _ prio) <- Map.toList sm]
        sorted = sortWith (Down . snd) pending -- Now > Normal
    case sorted of
      [] -> retry -- Block until status map changes
      (repo, _) : _ -> do
        -- Mark as InProgress and return
        modifyTVar' statusMap $ Map.insert repo (InProgress now)
        pure repo
