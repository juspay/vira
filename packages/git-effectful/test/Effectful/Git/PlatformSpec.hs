module Effectful.Git.PlatformSpec where

import Effectful.Git.Platform (GitPlatform (..), detectPlatform)
import Test.Hspec

spec :: Spec
spec = do
  describe "detectPlatform" $ do
    describe "GitHub detection" $ do
      it "detects github.com SSH URL" $ do
        detectPlatform "ssh://git@github.com/user/repo.git"
          `shouldBe` Just GitHub

      it "detects github.com HTTPS URL" $ do
        detectPlatform "https://github.com/user/repo.git"
          `shouldBe` Just GitHub

    describe "Bitbucket detection" $ do
      it "detects bitbucket.org SSH URL" $ do
        detectPlatform "ssh://git@bitbucket.org/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.org")

      it "detects bitbucket.org HTTPS URL" $ do
        detectPlatform "https://bitbucket.org/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.org")

      it "detects custom bitbucket SSH URL with ssh. prefix" $ do
        detectPlatform "ssh://git@ssh.bitbucket.juspay.net/xyne/xyne-spaces.git"
          `shouldBe` Just (Bitbucket "bitbucket.juspay.net")

      it "detects custom bitbucket SSH URL without ssh. prefix" $ do
        detectPlatform "ssh://git@bitbucket.juspay.net/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.juspay.net")

      it "detects custom bitbucket HTTPS URL" $ do
        detectPlatform "https://bitbucket.example.com/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.example.com")

      it "handles HTTP URLs" $ do
        detectPlatform "http://bitbucket.example.com/user/repo.git"
          `shouldBe` Just (Bitbucket "bitbucket.example.com")

    describe "Unknown platforms" $ do
      it "returns Nothing for gitlab.com" $ do
        detectPlatform "ssh://git@gitlab.com/user/repo.git"
          `shouldBe` Nothing

      it "returns Nothing for unknown host" $ do
        detectPlatform "ssh://git@example.com/user/repo.git"
          `shouldBe` Nothing
