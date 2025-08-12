{- | Pure Haskell file tailing implementation
Replaces external `tail -f` process with STM-coordinated file monitoring
-}
module Vira.Lib.FileTailer (
  FileTailer,
  startTailing,
  stopTailing,
  subscribeToTail,
  unsubscribeFromTail,
  tryReadTailQueue,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TBQueue)
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import System.Directory (doesFileExist)

-- | File tailer state
data FileTailer = FileTailer
  { filePath :: FilePath
  -- ^ Path to the file being tailed
  , lastPosition :: TVar Int
  -- ^ Last read line number (for tracking progress)
  , stopSignal :: STM.TMVar ()
  -- ^ Signal to stop tailing (empty = keep running, filled = stop)
  , clientQueues :: STM.TVar [TBQueue Text]
  -- ^ List of client queues to broadcast to
  }

-- | Start tailing a file
startTailing :: FilePath -> IO FileTailer
startTailing fp = do
  lastPosition <- STM.newTVarIO 0
  stopSignal <- STM.newEmptyTMVarIO
  clientQueues <- STM.newTVarIO []

  let tailer = FileTailer fp lastPosition stopSignal clientQueues

  -- Start the tailing thread
  void $ forkIO $ tailingLoop tailer

  pure tailer

-- | Stop tailing and clean up resources
stopTailing :: FileTailer -> IO ()
stopTailing tailer = do
  -- Signal the tailing thread to stop
  STM.atomically $ STM.putTMVar (stopSignal tailer) ()

  -- Wait a moment for graceful shutdown
  threadDelay 100_000

-- | Subscribe to tail output, returns a new queue for this client
subscribeToTail :: FileTailer -> IO (TBQueue Text)
subscribeToTail tailer = do
  clientQueue <- STM.newTBQueueIO 1000 -- Buffer up to 1000 lines per client
  STM.atomically $ do
    queues <- STM.readTVar (clientQueues tailer)
    STM.writeTVar (clientQueues tailer) (clientQueue : queues)
  pure clientQueue

-- | Unsubscribe from tail output
unsubscribeFromTail :: FileTailer -> TBQueue Text -> IO ()
unsubscribeFromTail tailer clientQueue = do
  STM.atomically $ do
    queues <- STM.readTVar (clientQueues tailer)
    STM.writeTVar (clientQueues tailer) (filter (/= clientQueue) queues)

-- | Try to read from a client's tail queue (non-blocking)
tryReadTailQueue :: TBQueue Text -> IO (Maybe Text)
tryReadTailQueue queue = do
  logLines <- STM.atomically $ STM.flushTBQueue queue
  if null logLines
    then pure Nothing
    else pure $ Just $ unlines logLines

-- | Main tailing loop - runs in its own thread
tailingLoop :: FileTailer -> IO ()
tailingLoop tailer = do
  let loop = do
        -- Check if we should stop
        shouldStop <- STM.atomically $ STM.tryReadTMVar (stopSignal tailer)
        case shouldStop of
          Just () -> do
            -- Stop signal received, broadcast final message and exit
            broadcastToAllClients tailer "<!-- End of log stream -->"
            pass
          Nothing -> do
            -- No stop signal, continue tailing
            readAndBroadcast tailer
            threadDelay 100_000 -- Poll every 100ms
            loop

  -- Handle exceptions gracefully
  loop `catch` \(e :: SomeException) -> do
    -- Log error and stop
    broadcastToAllClients tailer $ "<!-- Tailing error: " <> show e <> " -->"

-- | Read new content from file and broadcast to all clients
readAndBroadcast :: FileTailer -> IO ()
readAndBroadcast tailer = do
  let fp = filePath tailer

  -- Simple approach: try to read file line by line from current position
  result <-
    ( do
        -- Try to read the file each time (don't keep file handle open)
        exists <- doesFileExist fp
        if not exists
          then pure []
          else do
            content <- decodeUtf8 <$> readFileBS fp
            let allLines = lines content
            lastPos <- STM.readTVarIO (lastPosition tailer)
            let newLines = drop lastPos allLines
            unless (null newLines) $ do
              STM.atomically $ STM.writeTVar (lastPosition tailer) (length allLines)
            pure newLines
      )
      `catch` \(_ :: SomeException) -> pure []

  -- Broadcast new lines to all clients
  forM_ result $ \line ->
    broadcastToAllClients tailer line

-- | Broadcast a message to all connected clients
broadcastToAllClients :: FileTailer -> Text -> IO ()
broadcastToAllClients tailer message = do
  queues <- STM.readTVarIO (clientQueues tailer)
  forM_ queues $ \queue -> do
    STM.atomically $ STM.writeTBQueue queue message
