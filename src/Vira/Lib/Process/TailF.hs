-- | `tail -f` streaming in Haskell
module Vira.Lib.Process.TailF where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import System.IO (hGetLine)
import System.Process (CreateProcess (std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess, waitForProcess)

-- Represent a `tail -f` process along with its output gathered up in TChat
data TailF = TailF FilePath ProcessHandle (TQueue Text)

new :: FilePath -> IO TailF
new filePath = do
  queue <- newTQueueIO
  ph <- run filePath queue
  pure $ TailF filePath ph queue

run :: FilePath -> TQueue Text -> IO ProcessHandle
run filePath chan = do
  (_, Just hOut, _, ph) <-
    createProcess
      ( proc
          "tail"
          [ "-n"
          , "+1" -- This reads whole file; cf. https://askubuntu.com/a/509915/26624
          , "-f"
          , filePath
          ]
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

tryRead :: TailF -> IO (Maybe Text)
tryRead (TailF _ _ chan) = do
  ls <- atomically $ flushTQueue chan
  if null ls
    then pure Nothing
    else pure $ Just $ unlines ls

stop :: TailF -> IO ()
stop (TailF _ ph _) = do
  terminateProcess ph
  void $ waitForProcess ph
