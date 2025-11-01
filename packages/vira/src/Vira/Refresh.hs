{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Public interface for repository refresh operations
module Vira.Refresh (
  -- * Refresh operations
  scheduleRepoRefresh,
  getRepoRefreshStatus,
) where

import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (Info), log)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Git (RepoName)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Refresh.StatusMap qualified as StatusMap
import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), RefreshStatus (..))
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
  , Concurrent :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  RepoName ->
  RefreshPriority ->
  Eff es ()
scheduleRepoRefresh repo prio = do
  now <- liftIO getCurrentTime
  StatusMap.markRepoPending repo now prio
  log Info $ "Queued refresh with prio: " <> show prio
