{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Example: Vira CI Workflow Simulation

This example demonstrates the logsink library by simulating Vira's
multi-process logging architecture:

  * Master process logs orchestration events
  * Worker subprocesses with stdout/stderr drainage
  * Fan-out to both file and broadcast
  * Real-time stream reader consuming from broadcast

Run with: cabal run logsink-example
-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, withAsync)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.Aeson (ToJSON, encode)
import LogSink (Sink (..))
import LogSink.Broadcast (Broadcast (..), broadcastSink, newBroadcast)
import LogSink.File (fileSink)
import LogSink.Handle (drainHandleWith)
import System.Process (CreateProcess (..), StdStream (..), createProcess, shell, waitForProcess)

-- | Structured log entry (similar to Vira's ViraLog)
data LogEntry = LogEntry
  { severity :: Severity
  , message :: Text
  , context :: [(Text, Text)]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Severity = Debug | Info | Warning | Error
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

encodeLogEntry :: LogEntry -> Text
encodeLogEntry = decodeUtf8 . encode

main :: IO ()
main = do
  putTextLn "=== logsink Example: Vira CI Workflow Simulation ==="
  putTextLn ""

  -- Create broadcast sink (ring buffer of 100 lines) for Text (encoded JSON)
  broadcast <- newBroadcast 100

  -- Create file sink that writes Text lines
  fileSink' <- fileSink "output.log.jsonl"

  -- Create a Text sink that writes to both broadcast and file
  let textSink :: Sink Text
      textSink = broadcastSink broadcast <> fileSink'

  -- Create a LogEntry sink that encodes to Text, then writes to textSink
  let sink :: Sink LogEntry
      sink = contramap encodeLogEntry textSink

  -- Master process logs orchestration start
  sinkWrite sink $ LogEntry Info "Starting build orchestration" [("phase", "init")]

  -- Spawn stream reader BEFORE workers so it catches everything
  readerAsync <- async $ streamReader broadcast

  -- Small delay to ensure reader is subscribed
  threadDelay 100_000

  -- Spawn worker processes (simulating nix build, git clone, etc.)
  putTextLn "[MASTER] Spawning 3 worker processes..."
  workers <- mapM (\wid -> async $ runWorker wid sink) [1 .. 3]

  -- Wait for all workers
  mapM_ wait workers
  sinkWrite sink $ LogEntry Info "All workers complete" [("phase", "done")]

  -- Small delay to let reader catch up
  threadDelay 500_000

  -- Close broadcast (signals reader to stop)
  putTextLn "[MASTER] Closing broadcast..."
  bcClose broadcast
  sinkClose fileSink'

  -- Wait for reader to finish
  wait readerAsync

  putTextLn ""
  putTextLn "=== Done! Check output.log.jsonl for file output ==="

-- | Run a simulated worker process
runWorker :: Int -> Sink LogEntry -> IO ()
runWorker workerId sink = do
  sinkWrite sink $
    LogEntry Info ("Worker " <> show workerId <> " starting") [("worker", show workerId)]

  -- Simulate subprocess with stdout/stderr
  let cmd = "echo 'Worker " <> show workerId <> ": processing...'; sleep 0." <> show workerId <> "; echo 'Worker " <> show workerId <> ": done'"
  (_, Just stdoutH, Just stderrH, ph) <-
    createProcess $
      (shell cmd)
        { std_out = CreatePipe
        , std_err = CreatePipe
        }

  -- Drain both handles concurrently to sink
  -- Transform each line to LogEntry
  let toLogEntry :: Maybe Severity -> Text -> LogEntry
      toLogEntry mSev line =
        LogEntry
          (fromMaybe Info mSev)
          line
          [("worker", show workerId), ("source", "subprocess")]

  withAsync (drainHandleWith (toLogEntry Nothing) stdoutH sink) $ \_ ->
    withAsync (drainHandleWith (toLogEntry $ Just Error) stderrH sink) $ \_ ->
      void $ waitForProcess ph

  sinkWrite sink $
    LogEntry Info ("Worker " <> show workerId <> " finished") [("worker", show workerId)]

-- | Stream reader that consumes from broadcast
streamReader :: Broadcast Text -> IO ()
streamReader bc = do
  queue <- bc.bcSubscribe
  putTextLn "[READER] Subscribed to broadcast, streaming..."
  let loop = do
        result <- atomically $ CB.drain queue
        case result of
          Nothing -> putTextLn "[READER] --- Stream ended ---"
          Just entries -> do
            mapM_ (\entry -> putTextLn $ "[STREAM] " <> entry) entries
            loop
  loop
