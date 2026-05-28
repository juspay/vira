module GH.SignoffSpec where

import Data.Aeson qualified as Aeson
import GH.Core (GitHubRepo (..), PullRequest (..), githubRepoFromUrl)
import Test.Hspec

spec :: Spec
spec = do
  describe "gh-signoff" $ do
    it "placeholder test" $ do
      True `shouldBe` True

  describe "PullRequest" $ do
    it "decodes gh pr list JSON" $ do
      Aeson.eitherDecodeStrict "[{\"number\":42,\"title\":\"Link branch PR\",\"url\":\"https://github.com/juspay/vira/pull/42\"}]"
        `shouldBe` Right
          [ PullRequest
              { number = 42
              , title = "Link branch PR"
              , url = "https://github.com/juspay/vira/pull/42"
              }
          ]

    it "extracts owner and repository from HTTPS clone URLs" $ do
      githubRepoFromUrl "https://github.com/juspay/vira.git"
        `shouldBe` Just (GitHubRepo "juspay" "vira")

    it "extracts owner and repository from SCP-style SSH clone URLs" $ do
      githubRepoFromUrl "git@github.com:juspay/vira.git"
        `shouldBe` Just (GitHubRepo "juspay" "vira")

    it "rejects non-GitHub URLs" $ do
      githubRepoFromUrl "git@gitlab.com:juspay/vira.git"
        `shouldBe` Nothing
