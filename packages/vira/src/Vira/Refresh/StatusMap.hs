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
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.AcidState qualified as App
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Refresh.Type (RefreshPriority, RefreshResult, RefreshState (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (Reader, asks, atomically)

{- | Initialize refresh state from acid-state

Loads all repos' lastRefresh status into the TVar map. Called on daemon startup.
-}
initializeRefreshState :: (Reader ViraRuntimeState :> es, Concurrent :> es, IOE :> es) => Eff es ()
initializeRefreshState = do
  st <- asks (.refreshState)
  repos <- App.query St.GetAllReposA
  let initialStatus =
        Map.fromList
          [ (repo.name, Completed result)
          | repo <- repos
          , Just result <- [repo.lastRefresh]
          ]
  atomically $ writeTVar st.statusMap initialStatus

-- | Mark a repository as Pending with given priority
markRepoPending :: (Reader ViraRuntimeState :> es, Concurrent :> es) => RepoName -> UTCTime -> RefreshPriority -> Eff es ()
markRepoPending repo now prio = do
  st <- asks (.refreshState)
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Pending now prio)

-- | Mark a repository as Completed and persist to acid-state
markRepoCompleted :: (Reader ViraRuntimeState :> es, Concurrent :> es, IOE :> es) => RepoName -> RefreshResult -> Eff es ()
markRepoCompleted repo result = do
  st <- asks (.refreshState)
  -- Update TVar
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Completed result)
  -- Persist to acid-state
  App.update $ St.SetRefreshStatusA repo (Just result)

-- | Remove a repository from refresh state (cleanup on deletion)
removeRepoFromRefreshState :: (Reader ViraRuntimeState :> es, Concurrent :> es) => RepoName -> Eff es ()
removeRepoFromRefreshState repo = do
  st <- asks (.refreshState)
  atomically $ modifyTVar' st.statusMap (Map.delete repo)

{- | Atomically pop the next pending repo and mark it as InProgress
Blocks (via STM retry) when no pending repos available
-}
popNextPendingRepo :: (Reader ViraRuntimeState :> es, Concurrent :> es, IOE :> es) => Eff es RepoName
popNextPendingRepo = do
  st <- asks (.refreshState)
  now <- liftIO getCurrentTime
  atomically $ do
    statusMap <- readTVar st.statusMap
    -- Find all pending repos sorted by priority
    let pending = [(repo, prio) | (repo, Pending _ prio) <- Map.toList statusMap]
        sorted = sortWith (Down . snd) pending -- Now > Normal
    case sorted of
      [] -> retry -- Block until status map changes
      (repo, _) : _ -> do
        -- Mark as InProgress and return
        modifyTVar' st.statusMap $ Map.insert repo (InProgress now)
        pure repo
