{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- |
File tailing library with multi-subscriber support.

This library provides a Haskell API for @tail -f@ style file streaming
using 'STM', 'Control.Concurrent.Async', and system processes.

Example usage:

@
import System.Tail

main :: IO ()
main = do
  tail <- 'tailFile' 100 \"\/var\/log\/app.log\"
  subscriber <- 'tailSubscribe' tail
  -- Read from subscriber...
  'tailStop' tail
@
-}
module System.Tail (
  -- * Core Types
  Tail,

  -- * Operations
  tailFile,
  tailStop,
  tailSubscribe,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM.CircularBuffer (CircularBuffer)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import System.Directory (doesFileExist)
import System.IO (hGetLine)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess, waitForProcess)

-- | Represent `tail -f`'ing a file in Haskell
data Tail = Tail
  { filePath :: FilePath
  -- ^ The file being tailed
  , stop :: TMVar ()
  -- ^ Signal to stop tailing
  , tailProcess :: TVar (Maybe (ProcessHandle, Async ()))
  -- ^ The tail process handle and async reader
  , queues :: TVar [CircularBuffer Text]
  -- ^ Active subscriber queues
  , ringBuffer :: CircularBuffer Text
  -- ^ Ring buffer storing last N lines for new subscribers
  }
  deriving stock (Generic)

{- | Create a new 'Tail' handle for the given file path with specified buffer size.

The tail process starts immediately and begins reading from the file.
New subscribers will receive a ring buffer containing the last @bufferSize@ lines.
-}
tailFile :: (HasCallStack) => FilePath -> Int -> FilePath -> IO Tail
tailFile tailBin bufferSize filePath = do
  unlessM (doesFileExist filePath) $ error $ "File does not exist: " <> toText filePath
  queues <- newTVarIO mempty
  stop <- newEmptyTMVarIO
  tailProcess <- newTVarIO Nothing
  ringBuffer <- atomically $ CB.new bufferSize
  let t = Tail {..}
  -- Start the tail process immediately
  void $ async $ tailRun tailBin t
  pure t

{- | Signal the tail process to stop reading the file.

This will terminate the underlying @tail@ process and close all subscriber queues.
-}
tailStop :: Tail -> IO ()
tailStop t = do
  atomically $ putTMVar t.stop ()

tailRun :: FilePath -> Tail -> IO ()
tailRun tailBin t = do
  -- Start the tail -F process (show entire file from beginning)
  let createProc = (proc tailBin ["-F", "-n", "+1", t.filePath]) {std_out = CreatePipe}
  (_, Just hout, _, ph) <- createProcess createProc

  -- Start async reader that reads from tail process and distributes to queues
  readerAsync <- async $ readAndDistribute hout

  -- Store the process and reader
  atomically $ writeTVar t.tailProcess (Just (ph, readerAsync))

  -- Wait for stop signal
  atomically $ takeTMVar t.stop

  -- Clean up: terminate process and wait for reader to finish at EOF
  threadDelay 1_000_000 -- Give `tail -f` a second to flush any remaining lines
  terminateProcess ph
  void $ waitForProcess ph

  -- Reader will naturally stop when it reaches EOF

  -- Close all queues so readers can detect end of stream
  atomically $ do
    qs <- readTVar t.queues
    mapM_ CB.close qs

  atomically $ writeTVar t.tailProcess Nothing
  where
    readAndDistribute :: Handle -> IO ()
    readAndDistribute h = do
      let readLines = do
            hIsEOF h >>= \case
              True -> pass -- EOF reached, stop reading
              False -> do
                line <- toText <$> hGetLine h
                -- Add to ring buffer and distribute to all queues
                atomically $ do
                  -- Update ring buffer
                  CB.add line t.ringBuffer
                  -- Distribute to subscriber queues
                  qs <- readTVar t.queues
                  forM_ qs $ \q ->
                    CB.add line q
                readLines
      readLines

{- | Subscribe to tail output and receive a 'CircularBuffer' for reading lines.

The returned buffer will contain any previously read lines from the ring buffer,
plus all new lines as they are read from the file.

Use 'Control.Concurrent.STM.CircularBuffer.drain' to read from the buffer.
-}
tailSubscribe :: Tail -> IO (CircularBuffer Text)
tailSubscribe t = atomically $ do
  -- Clone ring buffer as CircularBuffer with buffered lines
  queue <- CB.clone t.ringBuffer
  -- Add to active subscribers
  modifyTVar' t.queues (queue :)
  pure queue
