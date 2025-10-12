{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core refresh logic for Git repositories
module Vira.Refresh.Core (
  -- * Refresh operations
  scheduleRefreshRepo,
) where

import Colog.Message (Message)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Git (RepoName)
import Vira.Lib.Logging (Severity (Info), log)
import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), RefreshStatus (..))

-- | Schedule a repository for refresh with given priority
scheduleRefreshRepo :: (IOE :> es, Log Message :> es) => RefreshState -> RepoName -> RefreshPriority -> Eff es ()
scheduleRefreshRepo st repo prio = do
  now <- liftIO getCurrentTime
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Pending now prio)
  log Info $ "🔄 Queued refresh for " <> toText repo <> " (priority: Now)"
