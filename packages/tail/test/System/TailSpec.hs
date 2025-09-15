module System.TailSpec where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.CircularBuffer (CircularBuffer)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import GHC.IO.Handle (hClose)
import Paths_tail (getDataFileName)
import System.IO.Temp (withSystemTempFile)
import System.Process (system)
import System.Tail qualified as Tail
import Test.Hspec

-- Helper to read all items from CircularBuffer until closed
drainAll :: CircularBuffer a -> IO [a]
drainAll q = do
  let go acc = do
        maybeItems <- atomically $ CB.drain q
        case maybeItems of
          Nothing -> pure acc -- Queue is closed
          Just items -> go (acc ++ toList items)
  go []

spec :: Spec
spec = describe "System.Tail" $ do
  it "hello" $ do
    t <- Tail.tailFile 100 "/dev/null"
    Tail.tailStop t
  it "streams a static file" $ do
    testFile <- getDataFileName "test-fixture.txt"
    t <- Tail.tailFile 100 testFile
    q <- Tail.tailSubscribe t
    threadDelay 1_000_000 >> Tail.tailStop t
    ls <- drainAll q
    viaNonEmpty head ls `shouldBe` Just "Line 1: This is a test fixture file"
    -- Find the last line from the lines we got
    let lastLine = viaNonEmpty last ls
    lastLine `shouldBe` Just "Line 5: End of test fixture"
  it "streams a log file being appended to by another process" $ do
    -- Create a file under a temp directory. Then spawn an external process that writes to it lines over time.
    withSystemTempFile "tail-spec" $ \tempFile h -> do
      hClose h
      void $ system $ "echo 'Hello' >> " <> tempFile
      t <- Tail.tailFile 100 tempFile
      q <- Tail.tailSubscribe t
      void $ forkIO $ do
        void $ system $ "echo 'World' >> " <> tempFile
        threadDelay 1_000_000 >> Tail.tailStop t
      -- Read everything and compare output
      ls <- drainAll q
      ls `shouldBe` ["Hello", "World"]
  it "ring buffer provides last lines to new subscribers" $ do
    withSystemTempFile "ring-buffer-spec" $ \tempFile h -> do
      hClose h
      void $ system $ "echo 'Line1' >> " <> tempFile
      t <- Tail.tailFile 100 tempFile
      q <- Tail.tailSubscribe t
      void $ system $ "echo 'Line2' >> " <> tempFile -- Write *after* subscribing
      threadDelay 1_000_000 >> Tail.tailStop t
      ls <- drainAll q
      ls `shouldBe` ["Line1", "Line2"]

-- End of file.
