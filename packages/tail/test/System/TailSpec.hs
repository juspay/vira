module System.TailSpec where

import Data.Text qualified as T
import System.IO (hClose, hPutStrLn)
import System.IO.Temp (withSystemTempFile)
import System.Tail
import Test.Hspec

spec :: Spec
spec = describe "System.Tail" $ do
  it "can start and stop tailing" $ do
    withSystemTempFile "test.log" $ \path handle -> do
      hPutStrLn handle "line1"
      hPutStrLn handle "line2"
      hClose handle

      tailHandle <- start path
      tailReader <- addReader tailHandle

      -- Should get the last lines
      result1 <- tryRead tailReader
      result1 `shouldSatisfy` \case
        Just line -> "line" `T.isInfixOf` line
        Nothing -> False

      stop tailHandle

  it "multiple readers get same data" $ do
    withSystemTempFile "test.log" $ \path handle -> do
      hPutStrLn handle "test line"
      hClose handle

      tailHandle <- start path
      tailReader1 <- addReader tailHandle
      tailReader2 <- addReader tailHandle

      -- Both readers should get data
      result1 <- tryRead tailReader1
      result2 <- tryRead tailReader2

      result1 `shouldSatisfy` \case
        Just line -> "test line" `T.isInfixOf` line
        Nothing -> False

      result2 `shouldSatisfy` \case
        Just line -> "test line" `T.isInfixOf` line
        Nothing -> False

      stop tailHandle
