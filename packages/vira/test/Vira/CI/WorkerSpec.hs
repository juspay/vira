{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.WorkerSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Test.Hspec
import Vira.State.Type (Job (..), JobId (..), JobStatus (..))

-- Import internal function for testing
import Vira.CI.Worker (selectJobsToStart)

-- Visual bindings for job statuses
(🏃) :: JobStatus
(🏃) = JobRunning

(⏳) :: JobStatus
(⏳) = JobPending

spec :: Spec
spec = describe "Vira.CI.Worker" $ do
  describe "selectJobsToStart" $ do
    it "respects max concurrent limit" $ do
      let jobs =
            mkJobs
              [ ((🏃), "test-repo", "main")
              , ((🏃), "test-repo", "dev")
              , ((⏳), "test-repo", "feature")
              , ((⏳), "test-repo", "hotfix")
              ]
      uncurry (selectJobsToStart 2) (partitionJobs jobs) `shouldBe` []

    it "fills available slots with FIFO order" $ do
      let jobs =
            mkJobs
              [ ((🏃), "test-repo", "main")
              , ((⏳), "test-repo", "dev")
              , ((⏳), "test-repo", "feature")
              , ((⏳), "test-repo", "hotfix")
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.branch) result `shouldBe` [BranchName "dev", BranchName "feature"]
    it "returns empty list when no pending jobs" $ do
      let jobs = mkJobs [((🏃), "test-repo", "main")]
      uncurry (selectJobsToStart 3) (partitionJobs jobs) `shouldBe` []

    it "returns empty list when already at limit" $ do
      let jobs =
            mkJobs
              [ ((🏃), "test-repo", "main")
              , ((🏃), "test-repo", "dev")
              , ((🏃), "test-repo", "feature")
              , ((⏳), "test-repo", "hotfix")
              ]
      uncurry (selectJobsToStart 3) (partitionJobs jobs) `shouldBe` []

    it "starts all pending when under limit" $ do
      let jobs =
            mkJobs
              [ ((⏳), "test-repo", "main")
              , ((⏳), "test-repo", "dev")
              ]
          result = uncurry (selectJobsToStart 5) (partitionJobs jobs)
      fmap (.branch) result `shouldBe` [BranchName "main", BranchName "dev"]

    it "sorts by creation time (FIFO)" $ do
      let jobs =
            mkJobs
              [ ((⏳), "test-repo", "main")
              , ((⏳), "test-repo", "dev")
              , ((⏳), "test-repo", "feature")
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.branch) result `shouldBe` [BranchName "main", BranchName "dev", BranchName "feature"]
    it "allows max 1 running job per (repo, branch) pair" $ do
      let jobs =
            mkJobs
              [ ((🏃), "test-repo", "main")
              , ((⏳), "test-repo", "main")
              , ((⏳), "test-repo", "dev")
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.branch) result `shouldBe` [BranchName "dev"] -- only dev starts, main blocked

-- Build jobs with auto-incrementing IDs and timestamps
mkJobs :: [(JobStatus, RepoName, BranchName)] -> [Job]
mkJobs specs = flip evalState 1 $ forM specs $ \(status, repo, branch) -> do
  n <- get
  put (n + 1)
  let time = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime ((n - 1) * 100))
  pure $
    Job
      { repo
      , branch
      , commit = CommitID "abc123"
      , jobId = JobId (fromIntegral n)
      , jobWorkingDir = "/tmp/job/" <> show n
      , jobStatus = status
      , jobCreatedTime = time
      }

-- Helper to partition jobs by status
partitionJobs :: [Job] -> ([Job], [Job])
partitionJobs jobs = (running, queued)
  where
    running = filter (\j -> case j.jobStatus of JobRunning -> True; _ -> False) jobs
    queued = filter (\j -> case j.jobStatus of JobPending -> True; _ -> False) jobs
