{-# LANGUAGE OverloadedRecordDot #-}

{- | Efficient pure Haskell file tailing implementation

This module provides a memory and CPU efficient alternative to external `tail -f`
processes, using fsnotify for file change detection and STM for coordination.

= Mechanism

The file tailer works by:

1. **fsnotify-based detection**: Uses filesystem events to detect file modifications,
   eliminating polling overhead and providing near-instant responsiveness
2. **Incremental reading**: Tracks byte offset and reads only new content since
   last read, minimizing memory usage even for large files
3. **STM coordination**: Uses TMVar for stop signaling and TVar for tracking
   multiple client subscription queues
4. **Broadcast distribution**: New lines are distributed to all subscribed
   client queues simultaneously
5. **Graceful shutdown**: Stop signals ensure clean termination and final
   message delivery to clients

= Features

* **Memory efficient**: Only reads new content (KBs vs MBs for large files)
* **CPU efficient**: No polling - only processes actual file changes
* **Responsive**: Near-instant detection of new log lines via filesystem events (fsnotify)
* **UTF-8 safe**: Handles encoding errors gracefully

= Limitations

* **Line-based only**: Assumes UTF-8 text files with line breaks; binary files
   or files without newlines are not supported
* **Single file**: Does not handle log rotation to different filenames
-}
module Vira.Lib.FileTailer (
  FileTailer,
  FileTailerConfig (..),
  defaultConfig,
  startTailing,
  stopTailing,
  subscribeToTail,
  tryReadTailQueue,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TBQueue)
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Data.ByteString qualified as BS
import System.FSNotify (Event (..), eventPath, watchDir, withManager)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek), hFileSize, hSeek)
import Vira.Lib.CircularBuffer qualified as CB

-- | Configuration for FileTailer with named fields to prevent parameter confusion
data FileTailerConfig = FileTailerConfig
  { recentLines :: Natural
  -- ^ Number of recent lines to buffer for new subscribers
  , batchSize :: Natural
  -- ^ Maximum lines per batch sent to clients
  , queueSize :: Natural
  -- ^ Maximum batches buffered per client queue
  }
  deriving stock (Show, Eq)

-- | Default configuration with sensible values
defaultConfig :: FileTailerConfig
defaultConfig =
  FileTailerConfig
    { recentLines = 100
    , batchSize = 50
    , queueSize = 100
    }

-- | Split a list into appropriately sized batches
batchLines :: Natural -> [Text] -> [NonEmpty Text]
batchLines _ [] = []
batchLines maxSize xs =
  let (batch, rest) = splitAt (fromIntegral maxSize) xs
   in case batch of
        (firstLine : others) -> (firstLine :| others) : batchLines maxSize rest
        [] -> []

-- | File tailer state with runtime configuration
data FileTailer = FileTailer
  { config :: FileTailerConfig
  -- ^ Configuration (immutable after creation)
  , filePath :: FilePath
  -- ^ Path to the file being tailed
  , lastOffset :: TVar Integer
  -- ^ Last read byte position (for incremental reading)
  , stopSignal :: STM.TMVar ()
  -- ^ Signal to stop tailing (empty = keep running, filled = stop)
  , clientQueues :: STM.TVar [TBQueue (NonEmpty Text)]
  -- ^ List of client queues to broadcast to (batched lines)
  , recentLinesBuffer :: STM.TVar (CB.CircularBuffer Text)
  -- ^ Buffer of recent lines (circular buffer, bounded by config.recentLines)
  }

-- | Start tailing a file with runtime configuration
startTailing :: FileTailerConfig -> FilePath -> IO FileTailer
startTailing config fp = do
  lastOffset <- STM.newTVarIO 0
  stopSignal <- STM.newEmptyTMVarIO
  clientQueues <- STM.newTVarIO []
  recentLinesBuffer <- STM.newTVarIO (CB.new config.recentLines)

  let tailer = FileTailer config fp lastOffset stopSignal clientQueues recentLinesBuffer

  -- Start the tailing thread
  void $ forkIO $ tailingLoop tailer

  pure tailer

-- | Stop tailing and clean up resources
stopTailing :: FileTailer -> IO ()
stopTailing tailer = do
  -- Signal the tailing thread to stop
  STM.atomically $ STM.putTMVar tailer.stopSignal ()

-- Cleanup happens automatically in tailingLoop via finally

{- | Subscribe to tail output, returns a new queue for this client
New subscribers immediately receive recent lines for context
-}
subscribeToTail :: FileTailer -> IO (TBQueue (NonEmpty Text))
subscribeToTail tailer = do
  let queueCapacity = fromIntegral tailer.config.queueSize
  clientQueue <- STM.newTBQueueIO queueCapacity
  STM.atomically $ do
    -- Add to client list
    queues <- STM.readTVar tailer.clientQueues
    STM.writeTVar tailer.clientQueues (clientQueue : queues)

    -- Send recent lines to new subscriber for context
    recentBuffer <- STM.readTVar tailer.recentLinesBuffer
    let recentLinesList = toList recentBuffer
    unless (null recentLinesList) $ do
      -- Split recent lines into appropriately sized batches
      forM_ (batchLines tailer.config.batchSize recentLinesList) $ \batch ->
        STM.writeTBQueue clientQueue batch
  pure clientQueue

-- | Try to read from a client's tail queue (non-blocking)
tryReadTailQueue :: TBQueue (NonEmpty Text) -> IO (Maybe (NonEmpty Text))
tryReadTailQueue queue = do
  STM.atomically $ STM.tryReadTBQueue queue

-- | Main tailing loop - runs in its own thread
tailingLoop :: FileTailer -> IO ()
tailingLoop tailer = do
  action `catch` \(e :: SomeException) -> do
    -- Log error and stop
    broadcastLinesToAllClients tailer ["<!-- Tailing error: " <> show e <> " -->"]
  where
    action = withManager $ \manager -> do
      -- Send startup message
      broadcastLinesToAllClients tailer ["<!-- FileTailer started (fsnotify mode) -->"]

      -- Read any existing content first
      readAndBroadcast tailer

      -- Start watching for file changes
      stopAction <-
        watchDir
          manager
          (takeDirectory tailer.filePath)
          (\event -> eventPath event == tailer.filePath)
          (\_ -> readAndBroadcast tailer)

      -- Block until stop signal
      STM.atomically $ STM.takeTMVar tailer.stopSignal

      -- Cleanup
      stopAction
      broadcastLinesToAllClients tailer ["<!-- End of log stream -->"]

-- | Read new content from file and broadcast to all clients
readAndBroadcast :: FileTailer -> IO ()
readAndBroadcast tailer = do
  result <- action
  -- Broadcast new lines to all clients in batches
  unless (null result) $ do
    broadcastLinesToAllClients tailer result
  where
    action = do
      withFile tailer.filePath ReadMode $ \handle -> do
        currentOffset <- STM.readTVarIO tailer.lastOffset
        fileSize <- hFileSize handle

        if currentOffset > fileSize
          then do
            -- File was truncated or rotated, start from beginning
            STM.atomically $ STM.writeTVar tailer.lastOffset 0
            hSeek handle AbsoluteSeek 0
          else
            hSeek handle AbsoluteSeek currentOffset

        -- Read only new content
        newContentBytes <- BS.hGetContents handle
        let newContent = decodeUtf8With lenientDecode newContentBytes
        let newLines = lines newContent

        unless (null newLines) $ do
          -- Update offset by the number of bytes we actually read
          let bytesRead = fromIntegral $ BS.length newContentBytes
          STM.atomically $ STM.writeTVar tailer.lastOffset (currentOffset + bytesRead)

        pure newLines

{- | Broadcast lines to all connected clients in batches
Also maintains a buffer of recent lines for new subscribers
-}
broadcastLinesToAllClients :: FileTailer -> [Text] -> IO ()
broadcastLinesToAllClients tailer logLines = do
  -- Update recent lines buffer first using CircularBuffer
  STM.atomically $ STM.modifyTVar tailer.recentLinesBuffer $ CB.append logLines

  -- Broadcast to all current clients
  queues <- STM.readTVarIO tailer.clientQueues
  forM_ (batchLines tailer.config.batchSize logLines) $ \batch -> do
    forM_ queues $ \queue -> do
      STM.atomically $ STM.writeTBQueue queue batch
