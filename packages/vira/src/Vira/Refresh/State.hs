{-# LANGUAGE OverloadedRecordDot #-}

-- | Refresh state manipulation functions
module Vira.Refresh.State (
  initialize,
  markPending,
  markCompleted,
  remove,
  popAndMarkInProgress,
) where

import Control.Concurrent.STM (retry)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Concurrent.STM (atomically)
import Effectful.Git (RepoName)
import Vira.Refresh.Type (RefreshPriority, RefreshResult, RefreshState (..), RefreshStatus (..))
import Vira.State.Type (Repo (..))
import Prelude hiding (atomically)

{- | Initialize refresh state from persisted repo data

Loads all repos' lastRefresh status into the TVar map. Called on daemon startup.
-}
initialize :: (Concurrent :> es) => RefreshState -> [Repo] -> Eff es ()
initialize st repos = do
  let initialStatus =
        Map.fromList
          [ (repo.name, Completed result)
          | repo <- repos
          , Just result <- [repo.lastRefresh]
          ]
  atomically $ writeTVar st.statusMap initialStatus

-- | Mark a repository as Pending with given priority
markPending :: (Concurrent :> es) => RefreshState -> RepoName -> UTCTime -> RefreshPriority -> Eff es ()
markPending st repo now prio = do
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Pending now prio)

-- | Mark a repository as Completed
markCompleted :: (Concurrent :> es) => RefreshState -> RepoName -> RefreshResult -> Eff es ()
markCompleted st repo result = do
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Completed result)

-- | Remove a repository from refresh state (cleanup on deletion)
remove :: (Concurrent :> es) => RefreshState -> RepoName -> Eff es ()
remove st repo = do
  atomically $ modifyTVar' st.statusMap (Map.delete repo)

{- | Atomically pop the next pending repo and mark it as InProgress
Blocks (via STM retry) when no pending repos available
-}
popAndMarkInProgress :: (Concurrent :> es, IOE :> es) => RefreshState -> Eff es RepoName
popAndMarkInProgress st = do
  now <- liftIO getCurrentTime
  atomically $ do
    sm <- readTVar st.statusMap
    -- Find all pending repos sorted by priority
    let pending = [(repo, prio) | (repo, Pending _ prio) <- Map.toList sm]
        sorted = sortWith (Down . snd) pending -- Now > Normal
    case sorted of
      [] -> retry -- Block until status map changes
      (repo, _) : _ -> do
        -- Mark as InProgress and return
        modifyTVar' st.statusMap $ Map.insert repo (InProgress now)
        pure repo
