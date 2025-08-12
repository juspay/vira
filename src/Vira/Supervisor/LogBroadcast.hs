-- | Log broadcasting functionality for sharing file tailing across multiple clients
module Vira.Supervisor.LogBroadcast (
  getOrCreateLogBroadcaster,
  subscribeToLogs,
  unsubscribeFromLogs,
  stopLogBroadcaster,
  tryReadLogQueue,
) where

import Control.Concurrent.MVar (modifyMVar)
import Control.Concurrent.STM (TBQueue)
import Vira.Lib.FileTailer qualified as FileTailer
import Vira.Supervisor.Type (LogBroadcaster (..), Task (..))

-- | Get existing or create new log broadcaster for a task
getOrCreateLogBroadcaster :: (MonadIO m) => Task -> FilePath -> m LogBroadcaster
getOrCreateLogBroadcaster task logFile = liftIO $ do
  modifyMVar (logBroadcaster task) $ \case
    Just broadcaster -> pure (Just broadcaster, broadcaster)
    Nothing -> do
      broadcaster <- createLogBroadcaster logFile
      pure (Just broadcaster, broadcaster)

-- | Create a new log broadcaster using FileTailer
createLogBroadcaster :: FilePath -> IO LogBroadcaster
createLogBroadcaster logFile = do
  tailer <- FileTailer.startTailing logFile
  pure $ LogBroadcaster tailer

-- | Subscribe to log broadcasts, returns a new queue for this client
subscribeToLogs :: (MonadIO m) => LogBroadcaster -> m (TBQueue Text)
subscribeToLogs broadcaster = liftIO $ do
  FileTailer.subscribeToTail (fileTailer broadcaster)

-- | Unsubscribe from log broadcasts
unsubscribeFromLogs :: (MonadIO m) => LogBroadcaster -> TBQueue Text -> m ()
unsubscribeFromLogs broadcaster clientQueue = liftIO $ do
  FileTailer.unsubscribeFromTail (fileTailer broadcaster) clientQueue

-- | Stop the log broadcaster and clean up resources
stopLogBroadcaster :: (MonadIO m) => LogBroadcaster -> m ()
stopLogBroadcaster broadcaster = liftIO $ do
  -- No delay needed! FileTailer handles graceful shutdown with stop signaling
  FileTailer.stopTailing (fileTailer broadcaster)

-- | Try to read from a client's log queue (non-blocking)
tryReadLogQueue :: (MonadIO m) => TBQueue Text -> m (Maybe Text)
tryReadLogQueue queue = liftIO $ do
  FileTailer.tryReadTailQueue queue
