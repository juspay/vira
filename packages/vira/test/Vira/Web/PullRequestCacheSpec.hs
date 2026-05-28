module Vira.Web.PullRequestCacheSpec (spec) where

import Effectful.Git (BranchName (..))
import GH.Core qualified as GH
import Test.Hspec
import Vira.Web.PullRequestCache

spec :: Spec
spec = describe "Vira.Web.PullRequestCache" $ do
  it "reuses a cached lookup within the TTL" $ do
    cache <- newPullRequestCacheWithTtl 60
    calls <- newIORef (0 :: Int)
    let fetch = do
          modifyIORef' calls (+ 1)
          pure GH.NoPullRequest

    firstLookup <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature") fetch
    secondLookup <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature") fetch

    firstLookup `shouldBe` PullRequestCacheResult GH.NoPullRequest PullRequestCacheMiss
    secondLookup `shouldBe` PullRequestCacheResult GH.NoPullRequest PullRequestCacheHit
    readIORef calls `shouldReturn` 1

  it "keeps different branches in separate cache entries" $ do
    cache <- newPullRequestCacheWithTtl 60
    calls <- newIORef (0 :: Int)
    let fetch = do
          modifyIORef' calls (+ 1)
          pure GH.NoPullRequest

    _ <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature-a") fetch
    _ <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature-b") fetch

    readIORef calls `shouldReturn` 2

  it "refetches expired entries" $ do
    cache <- newPullRequestCacheWithTtl 0
    calls <- newIORef (0 :: Int)
    let fetch = do
          modifyIORef' calls (+ 1)
          pure GH.NoPullRequest

    firstLookup <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature") fetch
    secondLookup <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature") fetch

    firstLookup `shouldBe` PullRequestCacheResult GH.NoPullRequest PullRequestCacheMiss
    secondLookup `shouldBe` PullRequestCacheResult GH.NoPullRequest PullRequestCacheMiss
    readIORef calls `shouldReturn` 2

  it "evicts old entries when the cache reaches its cap" $ do
    cache <- newPullRequestCacheWithTtlAndMaxEntries 60 1
    calls <- newIORef (0 :: Int)
    let fetch = do
          modifyIORef' calls (+ 1)
          pure GH.NoPullRequest

    _ <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature-a") fetch
    _ <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature-b") fetch
    evictedLookup <- resolvePullRequestWith cache "https://github.com/juspay/vira.git" (BranchName "feature-a") fetch

    evictedLookup `shouldBe` PullRequestCacheResult GH.NoPullRequest PullRequestCacheMiss
    readIORef calls `shouldReturn` 3
