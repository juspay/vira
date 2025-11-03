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
      let specs =
            [ ((🏃), "test-repo", "main", False)
            , ((🏃), "test-repo", "dev", False)
            , ((⏳), "test-repo", "feature", False)
            , ((⏳), "test-repo", "hotfix", False)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 2) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs

    it "fills available slots with FIFO order" $ do
      let specs =
            [ ((🏃), "test-repo", "main", False)
            , ((⏳), "test-repo", "dev", True)
            , ((⏳), "test-repo", "feature", True)
            , ((⏳), "test-repo", "hotfix", False)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs
    it "returns empty list when no pending jobs" $ do
      let specs = [((🏃), "test-repo", "main", False)]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs

    it "returns empty list when already at limit" $ do
      let specs =
            [ ((🏃), "test-repo", "main", False)
            , ((🏃), "test-repo", "dev", False)
            , ((🏃), "test-repo", "feature", False)
            , ((⏳), "test-repo", "hotfix", False)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs

    it "starts all pending when under limit" $ do
      let specs =
            [ ((⏳), "test-repo", "main", True)
            , ((⏳), "test-repo", "dev", True)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 5) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs

    it "sorts by creation time (FIFO)" $ do
      let specs =
            [ ((⏳), "test-repo", "main", True)
            , ((⏳), "test-repo", "dev", True)
            , ((⏳), "test-repo", "feature", True)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs
    it "allows max 1 running job per (repo, branch) pair" $ do
      let specs =
            [ ((🏃), "test-repo", "main", False)
            , ((⏳), "test-repo", "main", False) -- blocked by running main
            , ((⏳), "test-repo", "dev", True)
            ]
          jobs = mkJobs specs
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      result `shouldBe` expectedToStart specs jobs

-- Build jobs with auto-incrementing IDs and timestamps
-- The Bool indicates whether the job is expected to be started
mkJobs :: [(JobStatus, RepoName, BranchName, Bool)] -> [Job]
mkJobs specs = flip evalState 1 $ forM specs $ \(status, repo, branch, _) -> do
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

-- Extract expected jobs to start based on the Bool flag
expectedToStart :: [(JobStatus, RepoName, BranchName, Bool)] -> [Job] -> [Job]
expectedToStart specs allJobs = [job | (job, (_, _, _, shouldStart)) <- zip allJobs specs, shouldStart]

-- Helper to partition jobs by status
partitionJobs :: [Job] -> ([Job], [Job])
partitionJobs jobs = (running, queued)
  where
    running = filter (\j -> case j.jobStatus of JobRunning -> True; _ -> False) jobs
    queued = filter (\j -> case j.jobStatus of JobPending -> True; _ -> False) jobs
