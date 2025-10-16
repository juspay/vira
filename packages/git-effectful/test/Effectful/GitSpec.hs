module Effectful.GitSpec where

import Colog (LogAction (LogAction))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful (runEff)
import Effectful.Colog (runLogAction)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git
import Effectful.Git.Command.ForEachRef (remoteBranchesFromClone)
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Process (runProcess)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "Git" $ do
    it "remoteBranchesFromClone with fixtures" $ do
      withSystemTempDirectory "git-test" $ \tempDir -> do
        let cloneUrl = "https://github.com/srid/haskell.page-old.git"
            mirrorPath = tempDir </> "mirror"
        -- Clone the repository first
        mirrorResult <- runEff . runProcess . runErrorNoCallStack @Text . runLogAction (LogAction $ const pass) $ Mirror.syncMirror cloneUrl mirrorPath
        case mirrorResult of
          Left err -> expectationFailure $ "Failed to clone: " <> toString err
          Right () -> do
            -- Get branches from the cloned repository
            result <- runEff . runProcess . runErrorNoCallStack @Text . runLogAction (LogAction $ const pass) $ remoteBranchesFromClone mirrorPath
            case result of
              Left err -> expectationFailure $ "Failed to get branches: " <> toString err
              Right branches -> do
                let expected =
                      Map.fromList
                        [ ("gh-pages", Commit (CommitID "0532a9a379790e6962996c756316d28501fe8a7f") "deploy: 2d06bb9e9f9c7ec6a27bd688412df99333c074bb" (posixSecondsToUTCTime 1638762332) "srid" "srid@users.noreply.github.com")
                        , ("hercules-effects", Commit (CommitID "7a105d260227129f68f87f00051a61b686375b7a") "try again" (posixSecondsToUTCTime 1638755989) "Sridhar Ratnakumar" "srid@srid.ca")
                        , ("master", Commit (CommitID "2d06bb9e9f9c7ec6a27bd688412df99333c074bb") "cache refactor, and start monthlyhask" (posixSecondsToUTCTime 1638762125) "Sridhar Ratnakumar" "srid@srid.ca")
                        ]
                branches `shouldBe` expected
