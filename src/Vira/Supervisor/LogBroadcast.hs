-- | Log broadcasting functionality for sharing tail -f across multiple clients
module Vira.Supervisor.LogBroadcast (
  getOrCreateLogBroadcaster,
  subscribeToLogs,
  unsubscribeFromLogs,
  stopLogBroadcaster,
  tryReadLogQueue,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar)
import Control.Concurrent.STM (TBQueue)
import Control.Concurrent.STM qualified as STM
import Data.Text.IO (hGetLine)
import System.Process (
  CreateProcess (std_out),
  StdStream (CreatePipe),
  createProcess,
  proc,
  terminateProcess,
  waitForProcess,
 )
import Vira.Supervisor.Type (LogBroadcaster (..), Task (..))

-- | Get existing or create new log broadcaster for a task
getOrCreateLogBroadcaster :: (MonadIO m) => Task -> FilePath -> m LogBroadcaster
getOrCreateLogBroadcaster task logFile = liftIO $ do
  modifyMVar (logBroadcaster task) $ \case
    Just broadcaster -> pure (Just broadcaster, broadcaster)
    Nothing -> do
      broadcaster <- createLogBroadcaster logFile
      pure (Just broadcaster, broadcaster)

-- | Create a new log broadcaster
createLogBroadcaster :: FilePath -> IO LogBroadcaster
createLogBroadcaster logFile = do
  clientQueues <- STM.newTVarIO []

  -- Start tail -f process
  (_, Just hOut, _, ph) <-
    createProcess
      ( proc "tail" ["-n", "+1", "-f", logFile]
      )
        { std_out = CreatePipe
        }
  hSetBuffering hOut LineBuffering

  -- Fork thread to read from tail and broadcast to all client queues
  void $ forkIO $ do
    let loop = do
          eof <- hIsEOF hOut
          unless eof $ do
            line <- hGetLine hOut
            let lineText = toText line
            queues <- STM.atomically $ STM.readTVar clientQueues
            forM_ queues $ \queue -> do
              STM.atomically $ STM.writeTBQueue queue lineText
            loop
    loop

  pure $ LogBroadcaster clientQueues ph

-- | Subscribe to log broadcasts, returns a new queue for this client
subscribeToLogs :: (MonadIO m) => LogBroadcaster -> m (TBQueue Text)
subscribeToLogs broadcaster = liftIO $ do
  clientQueue <- STM.newTBQueueIO 1000 -- Buffer up to 1000 log lines per client
  STM.atomically $ do
    queues <- STM.readTVar (clientQueues broadcaster)
    STM.writeTVar (clientQueues broadcaster) (clientQueue : queues)
  pure clientQueue

-- | Unsubscribe from log broadcasts
unsubscribeFromLogs :: (MonadIO m) => LogBroadcaster -> TBQueue Text -> m ()
unsubscribeFromLogs broadcaster clientQueue = liftIO $ do
  STM.atomically $ do
    queues <- STM.readTVar (clientQueues broadcaster)
    STM.writeTVar (clientQueues broadcaster) (filter (/= clientQueue) queues)

-- | Stop the log broadcaster and clean up resources
stopLogBroadcaster :: (MonadIO m) => LogBroadcaster -> m ()
stopLogBroadcaster broadcaster = liftIO $ do
  terminateProcess (tailProcess broadcaster)
  void $ waitForProcess (tailProcess broadcaster)

-- | Try to read from a client's log queue (non-blocking)
tryReadLogQueue :: (MonadIO m) => TBQueue Text -> m (Maybe Text)
tryReadLogQueue queue = liftIO $ do
  logLines <- STM.atomically $ STM.flushTBQueue queue
  if null logLines
    then pure Nothing
    else pure $ Just $ unlines logLines
