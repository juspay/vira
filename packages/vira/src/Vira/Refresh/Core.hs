{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core refresh logic for Git repositories
module Vira.Refresh.Core (
  -- * Refresh operations
  scheduleRepoRefresh,
  initializeRefreshState,
  getRepoRefreshStatus,
) where

import Colog.Message (RichMessage)
import Data.Acid qualified as Acid
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Git (RepoName)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Lib.Logging (LogContext, Severity (Info), log)
import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (Reader, ask, asks)

-- | Get the current refresh status for a repository
getRepoRefreshStatus ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  RepoName ->
  Eff es RefreshStatus
getRepoRefreshStatus repo = do
  st <- asks @ViraRuntimeState (.refreshState)
  statusMap <- liftIO $ readTVarIO st.statusMap
  pure $ Map.findWithDefault NeverRefreshed repo statusMap

-- | Schedule a repository for refresh with given priority
scheduleRepoRefresh ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  RepoName ->
  RefreshPriority ->
  Eff es ()
scheduleRepoRefresh repo prio = do
  st <- asks @ViraRuntimeState (.refreshState)
  now <- liftIO getCurrentTime
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Pending now prio)
  log Info $ "Queued refresh for " <> toText repo <> " (priority: " <> show prio <> ")"

{- | Initialize refresh state from acid-state

Loads all repos' lastRefresh status into the TVar map. Called on daemon startup.
-}
initializeRefreshState :: Eff AppStack ()
initializeRefreshState = do
  acid <- asks (.acid)
  st <- asks (.refreshState)
  repos <- liftIO $ Acid.query acid St.GetAllReposA
  let initialStatus =
        Map.fromList
          [ (repo.name, Completed result)
          | repo <- repos
          , Just result <- [repo.lastRefresh]
          ]
  atomically $ writeTVar st.statusMap initialStatus
