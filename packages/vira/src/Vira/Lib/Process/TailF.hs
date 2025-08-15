-- | `tail -f` streaming in Haskell
module Vira.Lib.Process.TailF where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import System.IO (hGetLine)
import System.Process (
  CreateProcess (std_out),
  ProcessHandle,
  StdStream (CreatePipe),
  createProcess,
  proc,
  terminateProcess,
  waitForProcess,
 )
import System.Tail ()

-- | Represent a `tail -f` process along with its output gathered up in TQueue
data TailF = TailF FilePath ProcessHandle (TQueue Text)

-- | Spawn a new tail process
new :: FilePath -> IO TailF
new filePath = do
  queue <- newTQueueIO
  ph <- run filePath queue
  pure $ TailF filePath ph queue

tailFArgs :: Maybe Int -> FilePath -> [String]
tailFArgs lastNLines fp =
  -- "+1" will read whole file.
  -- cf. https://askubuntu.com/a/509915/26624
  let n = maybe "+1" show lastNLines
   in ["-n", n, "-f", fp]

run :: FilePath -> TQueue Text -> IO ProcessHandle
run filePath chan = do
  (_, Just hOut, _, ph) <-
    createProcess
      ( proc
          "tail"
          (tailFArgs Nothing filePath)
      )
        { std_out = CreatePipe
        }
  hSetBuffering hOut LineBuffering
  void $ forkIO $ do
    let loop =
          hIsEOF hOut >>= \case
            True -> pass
            False -> do
              line <- hGetLine hOut
              atomically $ writeTQueue chan $ toText line
              loop
    loop
  -- Give the process a chance to start up
  threadDelay 100_000
  pure ph

-- | Read from `tail -f` if available. Nothing if no logs are available to read.
tryRead :: TailF -> IO (Maybe Text)
tryRead (TailF _ _ chan) = do
  ls <- atomically $ flushTQueue chan
  if null ls
    then pure Nothing
    else pure $ Just $ unlines ls

-- | Stop the tail process, returning the unread lines (the last lines, generally)
stop :: TailF -> IO (Maybe Text)
stop t@(TailF _ ph _) = do
  -- HACK: Give the process a chance to finish writing to the queue
  -- If we don't do this, we will miss out on the last few lines of log.
  threadDelay 1000_000
  s <- tryRead t
  -- Then terminate.
  terminateProcess ph
  void $ waitForProcess ph
  pure s
