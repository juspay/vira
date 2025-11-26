module Effectful.Git.PlatformSpec where

import Effectful.Git.Platform (GitPlatform (..), parseAndDetect)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseAndDetect" $ do
    describe "GitHub detection" $ do
      it "detects github.com SSH URL" $ do
        parseAndDetect "ssh://git@github.com/user/repo.git"
          `shouldBe` Just GitHub

      it "detects github.com HTTPS URL" $ do
        parseAndDetect "https://github.com/user/repo.git"
          `shouldBe` Just GitHub

    describe "Bitbucket detection" $ do
      it "detects bitbucket.org SSH URL" $ do
        parseAndDetect "ssh://git@bitbucket.org/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.org")

      it "detects bitbucket.org HTTPS URL" $ do
        parseAndDetect "https://bitbucket.org/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.org")

      it "detects custom bitbucket SSH URL with ssh. prefix" $ do
        parseAndDetect "ssh://git@ssh.bitbucket.juspay.net/xyne/xyne-spaces.git"
          `shouldBe` Just (Bitbucket "bitbucket.juspay.net")

      it "detects custom bitbucket SSH URL without ssh. prefix" $ do
        parseAndDetect "ssh://git@bitbucket.juspay.net/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.juspay.net")

      it "detects custom bitbucket HTTPS URL" $ do
        parseAndDetect "https://bitbucket.example.com/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.example.com")

      it "handles HTTP URLs" $ do
        parseAndDetect "http://bitbucket.example.com/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.example.com")

    describe "Unknown platforms" $ do
      it "returns Nothing for gitlab.com" $ do
        parseAndDetect "ssh://git@gitlab.com/user/repo.git"
          `shouldBe` Nothing

      it "returns Nothing for unknown host" $ do
        parseAndDetect "ssh://git@example.com/user/repo.git"
          `shouldBe` Nothing
