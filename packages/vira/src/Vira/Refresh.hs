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
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (Info), log, withLogContext)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Git (RepoName)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Refresh.State qualified as State
import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), RefreshStatus (..))
import Prelude hiding (Reader, ask, asks)

-- | Get the current 'RefreshStatus' for a repository
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

-- | Schedule a repository for refresh with given 'RefreshPriority'
scheduleRepoRefresh ::
  ( Reader ViraRuntimeState :> es
  , Concurrent :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  [RepoName] ->
  RefreshPriority ->
  Eff es ()
scheduleRepoRefresh repoNames prio = do
  now <- liftIO getCurrentTime
  st <- asks (.refreshState)
  State.markPending st repoNames now prio
  withLogContext [("prio", show prio)] $
    log Info $
      "Queued refresh for repos: " <> T.intercalate ", " (toText <$> repoNames)
