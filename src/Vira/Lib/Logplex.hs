{- | Concurrent process output management for use with `System.Process`

In future, we may refactor or rewrite this based on a streaming system. For now, we want something that just works.
-}
module Vira.Lib.Logplex where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (TChan, dupTChan, newTChan, readTChan, writeTChan)
import Control.Exception (bracketOnError)
import Data.Text.IO (hGetLine, hPutStrLn)
import System.IO (hClose)
import System.IO.Utf8
import System.Process (createPipe)

data LogPlex = LogPlex
  { writeHandle :: Handle
  , readHandle :: Handle
  , historyVar :: TVar Text
  , streamChan :: TChan Text
  , file :: FilePath
  }

newLogPlex :: FilePath -> IO LogPlex
newLogPlex file = do
  (readHandle, writeHandle) <- createPipe
  historyVar <- newTVarIO ""
  streamChan <- atomically newTChan
  return $ LogPlex writeHandle readHandle historyVar streamChan file

runLogPlex :: LogPlex -> IO ()
runLogPlex logplex@(LogPlex writeHandle readHandle _historyVar streamChan file) = do
  hSetBuffering writeHandle LineBuffering
  hSetBuffering readHandle LineBuffering
  -- Create a TVar for accumulated history and a TChan for new messages
  void $
    concurrently
      (pipeToChan logplex)
      (writeTChanToFile streamChan file)

subscribe :: LogPlex -> IO (Text, TChan Text)
subscribe (LogPlex _ _ historyVar streamChan _) = atomically $ do
  history <- readTVar historyVar
  readerChan <- dupTChan streamChan
  return (history, readerChan)

write :: LogPlex -> Text -> IO ()
write = pushLine

-- Helper function to read lines, update history, and write to TChan
pipeToChan :: LogPlex -> IO ()
pipeToChan logplex@(LogPlex _ readHandle _historyVar _streamChan _) = do
  eof <- hIsEOF readHandle
  unless eof $ do
    line <- hGetLine readHandle
    pushLine logplex line
    pipeToChan logplex

pushLine :: LogPlex -> Text -> IO ()
pushLine (LogPlex _ _ historyVar streamChan _) line = atomically $ do
  modifyTVar' historyVar (\old -> old <> line <> "\n")
  writeTChan streamChan line

writeTChanToFile :: TChan Text -> FilePath -> IO ()
writeTChanToFile tchan file = do
  putStrLn "Trying to open log file ..."
  bracketOnError (openFile file WriteMode) hClose $ \h -> do
    void $ infinitely $ do
      putStrLn "Reading from TChan.."
      msg <- atomically $ readTChan tchan
      putStrLn $ "Okay, writing to log file from TChan: " <> toString msg
      hPutStrLn h msg
      hFlush h
