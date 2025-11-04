module Vira.CI.AutoBuildSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Test.Hspec
import Vira.CI.AutoBuild (selectJobsToCancel)
import Vira.State.Acid (BranchUpdate (BranchUpdate))
import Vira.State.Type (Job (..), JobId (..), JobResult (..), JobStatus (..))

-- Visual bindings for job statuses
(🏃) :: JobStatus
(🏃) = JobRunning

(⏳) :: JobStatus
(⏳) = JobPending

(❌) :: JobStatus
(❌) = JobFinished JobKilled (UTCTime (fromGregorian 2025 1 1) 0)

spec :: Spec
spec = describe "Vira.CI.AutoBuild" $ do
  describe "selectJobsToCancel" $ do
    it "cancels pending jobs for updated branch" $
      testAutoBuild
        (BranchUpdate "main" (Just "abc123") "def456")
        [ ((⏳), (❌), "vira", "main", "abc123") -- pending → cancelled
        , ((🏃), (🏃), "vira", "main", "old999") -- running → unchanged
        , ((⏳), (⏳), "vira", "dev", "xyz789") -- different branch → unchanged
        ]
    it "skips running jobs (can't cancel)" $
      testAutoBuild
        (BranchUpdate "main" (Just "abc123") "def456")
        [ ((🏃), (🏃), "vira", "main", "abc123")
        , ((🏃), (🏃), "vira", "dev", "xyz789")
        ]
    it "cancels all pending for same branch" $
      testAutoBuild
        (BranchUpdate "main" (Just "c3") "c4")
        [ ((⏳), (❌), "vira", "main", "c1")
        , ((⏳), (❌), "vira", "main", "c2")
        , ((⏳), (❌), "vira", "main", "c3")
        , ((⏳), (⏳), "vira", "dev", "c5")
        ]
    it "ignores different repos" $
      testAutoBuild
        (BranchUpdate "main" (Just "abc") "def")
        [ ((⏳), (⏳), "other-repo", "main", "abc")
        , ((⏳), (❌), "vira", "main", "abc")
        ]

-- Test auto-build logic (which jobs get cancelled for a branch update)
-- Format: (initialStatus, expectedStatus, repo, branch, commit)
testAutoBuild :: BranchUpdate -> [(JobStatus, JobStatus, RepoName, BranchName, CommitID)] -> IO ()
testAutoBuild update specs = do
  let jobs = mkJobs specs
      repo = "vira" -- Hard-coded repo for all tests
      toCancel = selectJobsToCancel jobs repo update
      expected = expectedCancelled specs jobs
  toCancel `shouldBe` expected

-- Build jobs with auto-incrementing IDs
mkJobs :: [(JobStatus, JobStatus, RepoName, BranchName, CommitID)] -> [Job]
mkJobs specs = flip evalState 1 $ forM specs $ \(initialStatus, _, repo, branchName, commit) -> do
  n <- get
  put (n + 1)
  let time = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime ((n - 1) * 100))
      job :: Job
      job =
        Job
          { repo
          , branch = branchName
          , commit
          , jobId = JobId (fromIntegral n)
          , jobWorkingDir = "/tmp/job/" <> show n
          , jobStatus = initialStatus
          , jobCreatedTime = time
          }
  pure job

-- Extract jobs that should be cancelled (expectedStatus is ❌)
expectedCancelled :: [(JobStatus, JobStatus, RepoName, BranchName, CommitID)] -> [Job] -> [Job]
expectedCancelled specs allJobs =
  [job | (job, (_, JobFinished JobKilled _, _, _, _)) <- zip allJobs specs]
