{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import LogSink (Sink (..))
import LogSink.Broadcast (Broadcast (..), broadcastSink, newBroadcast)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Sink" $ do
    it "mempty discards all writes" $ do
      sinkWrite mempty ("test" :: Text)
    -- No error means success

    it "Semigroup writes to both sinks" $ do
      var1 <- newIORef ([] :: [Text])
      var2 <- newIORef ([] :: [Text])
      let sink1 = Sink (\t -> modifyIORef var1 (t :)) pass pass
      let sink2 = Sink (\t -> modifyIORef var2 (t :)) pass pass
      let combined = sink1 <> sink2
      sinkWrite combined "hello"
      sinkWrite combined "world"
      r1 <- readIORef var1
      r2 <- readIORef var2
      r1 `shouldBe` ["world", "hello"]
      r2 `shouldBe` ["world", "hello"]

  describe "Broadcast" $ do
    -- NOTE: bcSubscribe clones the ring buffer, which calls drain and blocks
    -- if the buffer is empty. So we MUST write before subscribing.
    it "late subscribers get ring buffer history" $ do
      bc <- newBroadcast 10
      -- Write BEFORE subscribing (required - clone blocks on empty)
      atomically $ bcWrite bc ("early1" :: Text)
      atomically $ bcWrite bc "early2"
      -- Now subscribe - should get buffered items
      queue <- bcSubscribe bc
      -- Should have buffered lines - drain returns immediately
      result <- atomically $ CB.drain queue
      result `shouldBe` Just (pure "early1" <> pure "early2")

    it "new writes reach subscribers" $ do
      bc <- newBroadcast 10
      -- Write one item to make subscribe work (clone needs non-empty buffer)
      atomically $ bcWrite bc ("initial" :: Text)
      queue <- bcSubscribe bc
      -- Drain the initial item
      _ <- atomically $ CB.drain queue

      -- Write more and verify subscriber gets them
      atomically $ bcWrite bc "after1"
      atomically $ bcWrite bc "after2"
      result <- atomically $ CB.drain queue
      result `shouldBe` Just (pure "after1" <> pure "after2")

    it "close signals end to subscribers" $ do
      bc <- newBroadcast 10
      -- Write to allow subscribe
      atomically $ bcWrite bc ("line" :: Text)
      queue <- bcSubscribe bc

      -- Drain initial items
      _ <- atomically $ CB.drain queue

      -- Close the broadcast
      bcClose bc

      -- Wait a moment for close to propagate
      threadDelay 10_000

      -- Now drain should return Nothing (closed)
      result <- atomically $ CB.drain queue
      result `shouldBe` Nothing

    it "broadcastSink works as Sink" $ do
      bc <- newBroadcast 10
      -- Write before subscribe
      sinkWrite (broadcastSink bc) ("initial" :: Text)
      queue <- bcSubscribe bc
      -- Should have the initial write
      result <- atomically $ CB.drain queue
      result `shouldBe` Just (pure "initial")

      -- New writes via sink also work
      sinkWrite (broadcastSink bc) "via-sink"
      result2 <- atomically $ CB.drain queue
      result2 `shouldBe` Just (pure "via-sink")

  describe "Broadcast concurrency" $ do
    it "handles multiple concurrent writers" $ do
      bc <- newBroadcast 100
      -- Write before subscribe
      atomically $ bcWrite bc ("init" :: String)
      queue <- bcSubscribe bc
      _ <- atomically $ CB.drain queue -- drain init

      -- Spawn multiple writers
      let writeItem i = atomically $ bcWrite bc (("writer-" <> show i) :: String)
      writers <- mapM (async . writeItem) [1 .. 10 :: Int]
      mapM_ wait writers

      -- Small delay to ensure all writes complete
      threadDelay 10_000

      -- Should have all 10 lines
      result <- atomically $ CB.drain queue
      case result of
        Nothing -> expectationFailure "Expected lines but got Nothing"
        Just entries -> length entries `shouldBe` 10

    it "multiple subscribers receive same data" $ do
      bc <- newBroadcast 10
      -- Write before subscribing
      atomically $ bcWrite bc ("init" :: Text)
      queue1 <- bcSubscribe bc
      queue2 <- bcSubscribe bc

      -- Drain init from both
      _ <- atomically $ CB.drain queue1
      _ <- atomically $ CB.drain queue2

      -- Write new data
      atomically $ bcWrite bc "shared"

      -- Both should receive it
      r1 <- atomically $ CB.drain queue1
      r2 <- atomically $ CB.drain queue2
      r1 `shouldBe` Just (pure "shared")
      r2 `shouldBe` Just (pure "shared")
