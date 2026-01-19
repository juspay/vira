{-# LANGUAGE TypeApplications #-}

module Control.Concurrent.STM.CircularBufferSpec where

import Control.Concurrent.STM.CircularBuffer qualified as CB
import Test.Hspec

spec :: Spec
spec = describe "CircularBuffer" $ do
  it "adds and drains items" $ do
    result <- atomically $ do
      buf <- CB.new @String 5
      CB.add "first" buf
      CB.add "second" buf
      CB.drain buf
    result `shouldBe` Just ("first" :| ["second"])

  it "drops oldest when full" $ do
    result <- atomically $ do
      buf <- CB.new @String 2
      CB.add "1" buf
      CB.add "2" buf
      CB.add "3" buf -- Drops "1"
      CB.drain buf
    result `shouldBe` Just ("2" :| ["3"])

  it "returns Nothing when closed" $ do
    result <- atomically $ do
      buf <- CB.new @String 5
      CB.close buf
      CB.drain buf
    result `shouldBe` Nothing

  it "clone copies items to new buffer" $ do
    (original, cloned) <- atomically $ do
      buf <- CB.new @String 3
      CB.add "a" buf
      CB.add "b" buf
      clonedBuf <- CB.clone buf
      o <- CB.drain buf
      c <- CB.drain clonedBuf
      pure (o, c)
    original `shouldBe` Just ("a" :| ["b"])
    cloned `shouldBe` Just ("a" :| ["b"])
