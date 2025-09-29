module Effectful.GitSpec where

import Colog (LogAction (LogAction))
import Data.List (isSubsequenceOf)
import Effectful (runEff)
import Effectful.Colog (runLogAction)
import Effectful.Git
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CmdSpec (RawCommand, ShellCommand), CreateProcess (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Git" $ do
    it "remoteBranchesFromSharedClone with non-existent directory" $ do
      withSystemTempDirectory "git-test" $ \tempDir -> do
        let nonExistentDir = tempDir </> "nonexistent"
        result <- runEff . runLogAction (LogAction $ const pass) $ remoteBranchesFromSharedClone nonExistentDir
        case result of
          Left _ -> pass -- Expected behavior for non-existent directory
          Right _ -> expectationFailure "Should fail for non-existent directory"

    it "cloneAtCommit creates proper CreateProcess" $ do
      let cmd = cloneAtCommit "https://github.com/example/repo.git" "abc123" "/tmp/target"
      -- Verify the command is structured correctly
      case cmdspec cmd of
        RawCommand prog args -> do
          prog `shouldSatisfy` isSubsequenceOf "git"
          args `shouldContain` ["clone"]
          args `shouldContain` ["abc123"]
        ShellCommand _ -> expectationFailure "Expected RawCommand, got ShellCommand"

-- Just check that we get some branches, don't rely on exact commit hashes
