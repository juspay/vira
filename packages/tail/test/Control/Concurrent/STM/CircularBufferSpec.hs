module Control.Concurrent.STM.CircularBufferSpec where

import Control.Concurrent.STM.CircularBuffer qualified as CB
import Test.Hspec

spec :: Spec
spec = describe "CircularBuffer" $ do
  it "adds and drains items" $ do
    items <- atomically $ do
      buf <- CB.new 3
      CB.add ("a" :: Text) buf
      CB.add "b" buf
      CB.drain buf
    fmap toList items `shouldBe` Just ["a", "b"]

  it "drops oldest when full" $ do
    items <- atomically $ do
      buf <- CB.new 2
      CB.add ("a" :: Text) buf
      CB.add "b" buf
      CB.add "c" buf -- Should drop "a"
      CB.drain buf
    fmap toList items `shouldBe` Just ["b", "c"]

  it "clones buffer contents" $ do
    clonedItems <- atomically $ do
      buf <- CB.new 2
      CB.add ("x" :: Text) buf
      CB.add "y" buf
      cloned <- CB.clone buf
      CB.drain cloned
    fmap toList clonedItems `shouldBe` Just ["x", "y"]

  it "drains remaining items when closed" $ do
    result <- atomically $ do
      buf <- CB.new 2
      CB.add ("test" :: Text) buf
      CB.close buf
      CB.drain buf
    fmap toList result `shouldBe` Just ["test"]

  it "returns Nothing when closed and empty" $ do
    result <- atomically $ do
      buf <- CB.new 2
      CB.close buf
      CB.drain buf
    result `shouldBe` (Nothing :: Maybe (NonEmpty Text))

  it "throws error when cloning closed buffer" $ do
    atomically
      ( do
          buf <- CB.new 2
          CB.close buf
          CB.clone buf
      )
      `shouldThrow` anyErrorCall
