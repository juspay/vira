{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Pipeline.ImplementationSpec (spec) where

import Control.Exception (finally)
import Data.List (lookup)
import Effectful.Git (BranchName (..), CommitID (..))
import LogSink (Sink (..))
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import Test.Hspec
import Vira.CI.Context (CIMode (..), ViraContext (..))
import Vira.CI.Pipeline.PostBuildHook (HookError (..), hookEnvVars, runHook)
import Prelude hiding (id)

-- | Sink that throws away anything written, for tests that don't inspect subprocess output.
discardSink :: Sink Text
discardSink = Sink {sinkWrite = const pass, sinkFlush = pass, sinkClose = pass}

-- | Write @body@ to a fresh temp file, mark it user-executable, and return its path.
writeTempScript :: Text -> IO FilePath
writeTempScript body = do
  (path, h) <- openTempFile "/tmp" "vira-hook-script.sh"
  hClose h
  writeFileBS path (encodeUtf8 body)
  setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode)
  pure path

-- Test data
testContext :: ViraContext
testContext =
  ViraContext
    { branch = BranchName "main"
    , ciMode = FullBuild
    , commitId = CommitID "deadbeef"
    , cloneUrl = Just "https://example.com/test-repo.git"
    , repoDir = "/tmp"
    }

spec :: Spec
spec = describe "Vira.CI.Pipeline.Implementation" $ do
  describe "hookEnvVars" $ do
    it "sets VIRA_BRANCH from context" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_BRANCH" vars `shouldBe` Just "main"

    it "sets VIRA_COMMIT_ID from context" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_COMMIT_ID" vars `shouldBe` Just "deadbeef"

    it "sets VIRA_REPO_CLONE_URL to the raw clone URL" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_REPO_CLONE_URL" vars `shouldBe` Just "https://example.com/test-repo.git"

    it "omits VIRA_REPO_CLONE_URL when cloneUrl is absent" $ do
      let vars = hookEnvVars testContext {cloneUrl = Nothing}
      lookup "VIRA_REPO_CLONE_URL" vars `shouldBe` Nothing

  describe "runHook" $ do
    it "returns Right () for a successful script" $ do
      script <- writeTempScript "#!/bin/sh\nexit 0\n"
      result <- runHook script [] "/tmp" discardSink Nothing `finally` removeFile script
      result `shouldBe` Right ()

    it "returns Left with the exit code for a failing script" $ do
      script <- writeTempScript "#!/bin/sh\nexit 1\n"
      result <- runHook script [] "/tmp" discardSink Nothing `finally` removeFile script
      result `shouldBe` Left (HookExited 1)

    it "passes environment variables to the script" $ do
      (outPath, outHandle) <- openTempFile "/tmp" "vira-hook-out"
      hClose outHandle
      script <- writeTempScript $ "#!/bin/sh\necho \"$VIRA_BRANCH\" > " <> toText outPath <> "\n"
      ( do
          result <- runHook script [("VIRA_BRANCH", "staging")] "/tmp" discardSink Nothing
          result `shouldBe` Right ()
          content <- readFileBS outPath
          decodeUtf8 content `shouldContain` "staging"
        )
        `finally` (removeFile script >> removeFile outPath)

    it "times out when the script runs longer than the limit" $ do
      script <- writeTempScript "#!/bin/sh\nsleep 5\n"
      result <- runHook script [] "/tmp" discardSink (Just 200_000) `finally` removeFile script
      case result of
        Left (HookTimedOut _) -> pass
        Left other -> expectationFailure $ "Expected HookTimedOut, got " <> show other
        Right () -> expectationFailure "Expected timeout, hook returned Right ()"

    it "hook envVars override the inherited environment" $ do
      let varName = "VIRA_TEST_PARENT_OVERRIDE_XYZ"
      setEnv varName "from-parent"
      (outPath, outHandle) <- openTempFile "/tmp" "vira-hook-out"
      hClose outHandle
      script <- writeTempScript $ "#!/bin/sh\necho \"$" <> toText varName <> "\" > " <> toText outPath <> "\n"
      ( do
          result <- runHook script [(toText varName, "from-hook")] "/tmp" discardSink Nothing
          result `shouldBe` Right ()
          content <- readFileBS outPath
          decodeUtf8 content `shouldContain` "from-hook"
        )
        `finally` (removeFile script >> removeFile outPath >> unsetEnv varName)
