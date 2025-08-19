{- | Simple tail -f streaming with multiple readers and stop control

API endpoints:
* 'start' - Begin tailing a file, returns 'TailHandle'
* 'addReader' - Add a new reader to existing tail, gets last N lines + new lines
* 'tryRead' - Non-blocking read from reader queue
* 'stop' - Stop tailing and flush remaining lines to all readers
* 'closeReader' - Remove a reader
-}
module System.Tail (
  TailHandle,
  TailReader,
  start,
  addReader,
  tryRead,
  stop,
  closeReader,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Exception (IOException, catch)
import System.IO (hGetLine)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess)

-- Constants
maxLastLines :: Int
maxLastLines = 10 -- Keep last N lines for new readers

readerQueueSize :: Natural
readerQueueSize = 100 -- Queue size per reader

startupDelayMicros :: Int
startupDelayMicros = 50_000 -- Startup delay for tail process

flushDelayMicros :: Int
flushDelayMicros = 100_000 -- Delay to flush final lines on stop

waitDelayMicros :: Int
waitDelayMicros = 100_000 -- EOF wait delay

-- | Handle for a tail process with multiple readers
data TailHandle = TailHandle
  { thProcessHandle :: ProcessHandle
  , thReaders :: STM.TVar [TailReader]
  , thLastLines :: STM.TVar [Text] -- Ring buffer, newest first
  , thShouldStop :: STM.TVar Bool
  }

-- | Reader handle for consuming tail output
newtype TailReader = TailReader (STM.TBQueue Text)

-- | Start tailing a file. Keeps last 10 lines for new readers.
start :: FilePath -> IO TailHandle
start filePath = do
  readers <- STM.newTVarIO []
  lastLines <- STM.newTVarIO []
  shouldStop <- STM.newTVarIO False

  (_, Just hOut, _, ph) <- createProcess (proc "tail" ["-n", "+1", "-f", filePath]) {std_out = CreatePipe}
  hSetBuffering hOut LineBuffering

  let handle = TailHandle ph readers lastLines shouldStop

  void $ forkIO $ tailLoop hOut handle
  threadDelay startupDelayMicros -- Let process start
  pure handle

-- | Add a new reader. Gets last 10 lines immediately, then new lines as they arrive.
addReader :: TailHandle -> IO TailReader
addReader handle = do
  queue <- STM.newTBQueueIO readerQueueSize
  let tailReader = TailReader queue

  STM.atomically $ do
    -- Send last lines to new reader
    lastLines <- STM.readTVar (thLastLines handle)
    mapM_ (STM.writeTBQueue queue) (reverse lastLines) -- Chronological order

    -- Add to readers list
    STM.modifyTVar' (thReaders handle) (tailReader :)

  pure tailReader

-- | Non-blocking read from reader queue
tryRead :: TailReader -> IO (Maybe Text)
tryRead (TailReader queue) = STM.atomically $ do
  isEmpty <- STM.isEmptyTBQueue queue
  if isEmpty then pure Nothing else Just <$> STM.readTBQueue queue

-- | Stop tailing and flush remaining lines to all readers
stop :: TailHandle -> IO ()
stop handle = do
  STM.atomically $ STM.writeTVar (thShouldStop handle) True
  threadDelay flushDelayMicros -- Let final lines flush
  terminateProcess (thProcessHandle handle)

-- | Remove a reader (automatic cleanup on full queue)
closeReader :: TailReader -> IO ()
closeReader _ = pass -- Readers auto-removed when queue full

-- Internal: Main tail reading loop
tailLoop :: Handle -> TailHandle -> IO ()
tailLoop hOut handle = do
  shouldStop <- STM.atomically $ STM.readTVar (thShouldStop handle)
  if shouldStop
    then flushAndExit
    else do
      result <- catch readLine handleError
      when result $ tailLoop hOut handle
  where
    readLine = do
      eof <- hIsEOF hOut
      if eof
        then do
          threadDelay waitDelayMicros -- Wait for more data
          pure True
        else do
          line <- hGetLine hOut
          let textLine = toText line
          STM.atomically $ do
            -- Update ring buffer (keep last N lines)
            STM.modifyTVar' (thLastLines handle) $ \currentLines ->
              take maxLastLines (textLine : currentLines)
            -- Send to all active readers
            sendToReaders textLine
          pure True

    handleError (_ :: IOException) = pure False

    flushAndExit = do
      -- Try to read any remaining lines
      catchRemaining
      where
        catchRemaining = do
          eof <- hIsEOF hOut
          if eof
            then pass
            else do
              line <- catch (Just . toText <$> hGetLine hOut) (\(_ :: IOException) -> pure Nothing)
              case line of
                Nothing -> pass
                Just textLine -> do
                  STM.atomically $ sendToReaders textLine
                  catchRemaining

    sendToReaders line = do
      readers <- STM.readTVar (thReaders handle)
      activeReaders <- filterM (canSend line) readers
      STM.writeTVar (thReaders handle) activeReaders

    canSend line (TailReader queue) = do
      full <- STM.isFullTBQueue queue
      if full
        then pure False -- Remove full readers
        else do
          STM.writeTBQueue queue line
          pure True
