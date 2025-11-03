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

____ :: Bool
____ = False

spec :: Spec
spec = describe "Vira.CI.Worker" $ do
  describe "selectJobsToStart" $ do
    it "respects max concurrent limit" $
      runBuildQueueTest
        2
        [ ((🏃), ____, "test-repo", "main")
        , ((🏃), ____, "test-repo", "dev")
        , ((⏳), ____, "test-repo", "feature")
        , ((⏳), ____, "test-repo", "hotfix")
        ]

    it "fills available slots with FIFO order" $
      runBuildQueueTest
        3
        [ ((🏃), ____, "test-repo", "main")
        , ((⏳), True, "test-repo", "dev")
        , ((⏳), True, "test-repo", "feature")
        , ((⏳), ____, "test-repo", "hotfix")
        ]

    it "starts all pending when under limit" $
      runBuildQueueTest
        5
        [ ((⏳), True, "test-repo", "main")
        , ((⏳), True, "test-repo", "dev")
        ]

    it "allows max 1 running job per (repo, branch) pair" $
      runBuildQueueTest
        3
        [ ((🏃), ____, "test-repo", "main")
        , ((⏳), ____, "test-repo", "main") -- blocked by running main
        , ((⏳), True, "test-repo", "dev")
        ]

-- Run a build queue test with given max concurrent limit and job specs
runBuildQueueTest :: Int -> [(JobStatus, Bool, RepoName, BranchName)] -> IO ()
runBuildQueueTest maxConcurrent specs = do
  let jobs = mkJobs specs
      result = uncurry (selectJobsToStart maxConcurrent) (partitionJobs jobs)
  result `shouldBe` expectedToStart specs jobs

-- Build jobs with auto-incrementing IDs and timestamps
-- The Bool indicates whether the job is expected to be started
mkJobs :: [(JobStatus, Bool, RepoName, BranchName)] -> [Job]
mkJobs specs = flip evalState 1 $ forM specs $ \(status, _, repo, branch) -> do
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
expectedToStart :: [(JobStatus, Bool, RepoName, BranchName)] -> [Job] -> [Job]
expectedToStart specs allJobs = [job | (job, (_, shouldStart, _, _)) <- zip allJobs specs, shouldStart]

-- Helper to partition jobs by status
partitionJobs :: [Job] -> ([Job], [Job])
partitionJobs jobs = (running, queued)
  where
    running = filter (\j -> case j.jobStatus of JobRunning -> True; _ -> False) jobs
    queued = filter (\j -> case j.jobStatus of JobPending -> True; _ -> False) jobs
