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
      let running = [mkJob 1 JobRunning t1, mkJob 2 JobRunning t2]
          pending = [mkJob 3 JobPending t3, mkJob 4 JobPending t4]
          jobs = running <> pending
      selectJobsToStart 2 jobs `shouldBe` []

    it "fills available slots with FIFO order" $ do
      let running = [mkJob 1 JobRunning t1]
          pending = [mkJob 2 JobPending t3, mkJob 3 JobPending t2, mkJob 4 JobPending t4]
          jobs = running <> pending
          result = selectJobsToStart 3 jobs
      length result `shouldBe` 2
      map (. jobId) result `shouldBe` [JobId 3, JobId 2] -- t2 < t3
    it "returns empty list when no pending jobs" $ do
      let running = [mkJob 1 JobRunning t1]
      selectJobsToStart 3 running `shouldBe` []

    it "returns empty list when already at limit" $ do
      let running = [mkJob 1 JobRunning t1, mkJob 2 JobRunning t2, mkJob 3 JobRunning t3]
          pending = [mkJob 4 JobPending t4]
          jobs = running <> pending
      selectJobsToStart 3 jobs `shouldBe` []

    it "starts all pending when under limit" $ do
      let pending = [mkJob 1 JobPending t1, mkJob 2 JobPending t2]
      let result = selectJobsToStart 5 pending
      length result `shouldBe` 2

    it "sorts by creation time (FIFO)" $ do
      let pending = [mkJob 1 JobPending t4, mkJob 2 JobPending t1, mkJob 3 JobPending t3]
      let result = selectJobsToStart 3 pending
      map (. jobId) result `shouldBe` [JobId 2, JobId 3, JobId 1] -- t1 < t3 < t4

-- Test fixtures
t1, t2, t3, t4 :: UTCTime
t1 = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
t2 = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 100)
t3 = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 200)
t4 = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 300)

mkJob :: Natural -> JobStatus -> UTCTime -> Job
mkJob n status time =
  Job
    { repo = RepoName "test-repo"
    , branch = BranchName "main"
    , commit = CommitID "abc123"
    , jobId = JobId n
    , jobWorkingDir = "/tmp/job/" <> show n
    , jobStatus = status
    , jobCreatedTime = time
    }
