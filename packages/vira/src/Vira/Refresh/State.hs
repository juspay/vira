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

{- | Initialize 'RefreshState' from persisted 'Vira.State.Type.Repo' data

Loads all repos' @lastRefresh@ status into the 'TVar' map. Called by 'Vira.Refresh.Daemon.startRefreshDaemon' on daemon startup.
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

-- | Mark repositories as 'Vira.Refresh.Type.Pending' with given 'RefreshPriority'
markPending :: (Concurrent :> es) => RefreshState -> [RepoName] -> UTCTime -> RefreshPriority -> Eff es ()
markPending st repos now prio = do
  atomically $ modifyTVar' st.statusMap $ \m ->
    foldl' (\acc repo -> Map.insert repo (Pending now prio) acc) m repos

-- | Mark a repository as 'Vira.Refresh.Type.Completed' with 'RefreshResult'
markCompleted :: (Concurrent :> es) => RefreshState -> RepoName -> RefreshResult -> Eff es ()
markCompleted st repo result = do
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Completed result)

-- | Remove a repository from 'RefreshState' (cleanup on deletion)
remove :: (Concurrent :> es) => RefreshState -> RepoName -> Eff es ()
remove st repo = do
  atomically $ modifyTVar' st.statusMap (Map.delete repo)

{- | Atomically pop the next pending repo and mark it as 'Vira.Refresh.Type.InProgress'

Blocks (via STM retry) when no 'Vira.Refresh.Type.Pending' repos available.
Repos are processed in priority order ('Vira.Refresh.Type.Now' before 'Vira.Refresh.Type.Normal').
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
