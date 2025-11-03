{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.WorkerSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Test.Hspec
import Vira.State.Type (Job (..), JobId (..), JobStatus (..))

-- Import internal function for testing
import Vira.CI.Worker (selectJobsToStart)

spec :: Spec
spec = describe "Vira.CI.Worker" $ do
  describe "selectJobsToStart" $ do
    it "respects max concurrent limit" $ do
      let jobs =
            mkJobs
              [ mkJob JobRunning "test-repo" "main"
              , mkJob JobRunning "test-repo" "dev"
              , mkJob JobPending "test-repo" "feature"
              , mkJob JobPending "test-repo" "hotfix"
              ]
      uncurry (selectJobsToStart 2) (partitionJobs jobs) `shouldBe` []

    it "fills available slots with FIFO order" $ do
      let jobs =
            mkJobs
              [ mkJob JobRunning "test-repo" "main"
              , mkJob JobPending "test-repo" "dev"
              , mkJob JobPending "test-repo" "feature"
              , mkJob JobPending "test-repo" "hotfix"
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.jobId) result `shouldBe` [JobId 2, JobId 3] -- FIFO order
    it "returns empty list when no pending jobs" $ do
      let jobs = mkJobs [mkJob JobRunning "test-repo" "main"]
      uncurry (selectJobsToStart 3) (partitionJobs jobs) `shouldBe` []

    it "returns empty list when already at limit" $ do
      let jobs =
            mkJobs
              [ mkJob JobRunning "test-repo" "main"
              , mkJob JobRunning "test-repo" "dev"
              , mkJob JobRunning "test-repo" "feature"
              , mkJob JobPending "test-repo" "hotfix"
              ]
      uncurry (selectJobsToStart 3) (partitionJobs jobs) `shouldBe` []

    it "starts all pending when under limit" $ do
      let jobs =
            mkJobs
              [ mkJob JobPending "test-repo" "main"
              , mkJob JobPending "test-repo" "dev"
              ]
          result = uncurry (selectJobsToStart 5) (partitionJobs jobs)
      fmap (.jobId) result `shouldBe` [JobId 1, JobId 2]

    it "sorts by creation time (FIFO)" $ do
      let jobs =
            mkJobs
              [ mkJob JobPending "test-repo" "main"
              , mkJob JobPending "test-repo" "dev"
              , mkJob JobPending "test-repo" "feature"
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.jobId) result `shouldBe` [JobId 1, JobId 2, JobId 3] -- FIFO order
    it "allows max 1 running job per (repo, branch) pair" $ do
      let jobs =
            mkJobs
              [ mkJob JobRunning "test-repo" "main"
              , mkJob JobPending "test-repo" "main"
              , mkJob JobPending "test-repo" "dev"
              ]
          result = uncurry (selectJobsToStart 3) (partitionJobs jobs)
      fmap (.jobId) result `shouldBe` [JobId 3] -- only dev starts, main blocked

-- State monad for building jobs with auto-incrementing IDs
type JobBuilder = State Integer

-- Build jobs with auto-incrementing IDs
mkJobs :: [JobBuilder Job] -> [Job]
mkJobs = flip evalState 1 . sequence

-- Helper to partition jobs by status
partitionJobs :: [Job] -> ([Job], [Job])
partitionJobs jobs = (running, queued)
  where
    running = filter (\j -> case j.jobStatus of JobRunning -> True; _ -> False) jobs
    queued = filter (\j -> case j.jobStatus of JobPending -> True; _ -> False) jobs

-- Creates job with auto-incrementing ID and timestamp (100s intervals)
mkJob :: JobStatus -> Text -> Text -> JobBuilder Job
mkJob status repoName branchName = do
  n <- get
  put (n + 1)
  let time = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime ((n - 1) * 100))
  pure $
    Job
      { repo = RepoName repoName
      , branch = BranchName branchName
      , commit = CommitID "abc123"
      , jobId = JobId (fromIntegral n)
      , jobWorkingDir = "/tmp/job/" <> show n
      , jobStatus = status
      , jobCreatedTime = time
      }
