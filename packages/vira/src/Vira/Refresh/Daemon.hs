{-# LANGUAGE OverloadedRecordDot #-}

-- | Refresh daemon for automatic repository updates
module Vira.Refresh.Daemon (
  startRefreshDaemon,
) where

import Effectful (Eff)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Reader.Dynamic (asks)
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Lib.Logging
import Vira.Refresh.Type (RefreshState (..))
import Prelude hiding (asks, atomically)

-- | Start the refresh daemon (dummy implementation for Phase 3a)
startRefreshDaemon :: Eff AppStack ()
startRefreshDaemon = do
  log Info "🔄 Refresh daemon starting (dummy implementation)..."

  -- Get refresh state from Reader
  st <- asks (.refreshState)

  -- Spawn a thread that sleeps forever and store handle
  handle <- async $ threadDelay maxBound
  atomically $ writeTVar st.daemonHandle (Just handle)

  log Info "🔄 Refresh daemon started"
