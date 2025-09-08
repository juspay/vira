module Effectful.GitSpec where

import Colog (LogAction (LogAction))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful (runEff)
import Effectful.Colog (runLogAction)
import Effectful.Git
import Test.Hspec

spec :: Spec
spec = do
  describe "Git" $ do
    it "remoteBranches" $ do
      branches <- runEff . runLogAction (LogAction $ const pass) $ remoteBranches "https://github.com/srid/haskell.page-old.git"
      let expected =
            Map.fromList
              [ ("gh-pages", Commit "0532a9a379790e6962996c756316d28501fe8a7f" "deploy: 2d06bb9e9f9c7ec6a27bd688412df99333c074bb" (posixSecondsToUTCTime 1638762332) "srid" "srid@users.noreply.github.com")
              , ("hercules-effects", Commit "7a105d260227129f68f87f00051a61b686375b7a" "try again" (posixSecondsToUTCTime 1638755989) "Sridhar Ratnakumar" "srid@srid.ca")
              , ("master", Commit "2d06bb9e9f9c7ec6a27bd688412df99333c074bb" "cache refactor, and start monthlyhask" (posixSecondsToUTCTime 1638762125) "Sridhar Ratnakumar" "srid@srid.ca")
              ]
      branches `shouldBe` expected
