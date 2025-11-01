{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.State.Type where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.IxSet.Typed
import Data.SafeCopy
import Data.Time (UTCTime)
import Effectful.Git (BranchName, Commit (..), CommitID, IxCommit, RepoName (..))
import Servant.API (FromHttpApiData, ToHttpApiData)
import Vira.Refresh.Type (RefreshResult)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

-- | A project's git repository
data Repo = Repo
  { name :: RepoName
  -- ^ An unique name identifying this repository
  , cloneUrl :: Text
  -- ^ The git clone URL of the repository
  , lastRefresh :: Maybe RefreshResult
  -- ^ Metadata about the last refresh operation (persisted across restarts)
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- TODO: type-check field names during compile-time
instance FromForm Repo where
  fromForm f =
    Repo
      <$> parseUnique "name" f
      <*> parseUnique "cloneUrl" f
      <*> pure Nothing -- lastRefresh not set via form

type RepoIxs = '[RepoName]
type IxRepo = IxSet RepoIxs Repo

instance Indexable RepoIxs Repo where
  indices = ixList (ixFun $ \Repo {name} -> [name])

data Branch = Branch
  { repoName :: RepoName
  -- ^ The name of the repository this branch belongs to
  , branchName :: BranchName
  -- ^ The name of the branch
  , headCommit :: Commit
  -- ^ The commit at the head of the branch
  , deleted :: Bool
  -- ^ Whether this branch has been deleted from the remote
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

type BranchIxs = '[RepoName, BranchName]
type IxBranch = IxSet BranchIxs Branch

instance Indexable BranchIxs Branch where
  indices =
    ixList
      (ixFun $ \Branch {repoName} -> [repoName])
      (ixFun $ \Branch {branchName} -> [branchName])

-- | Badge state for branch status
data BadgeState = NeverBuilt | OutOfDate
  deriving stock (Generic, Show, Eq)

-- | Branch with enriched metadata for display
data BranchDetails = BranchDetails
  { branch :: Branch
  -- ^ The branch information from the database
  , mLatestJob :: Maybe Job
  -- ^ The most recent CI job for this branch, if any
  , jobsCount :: Natural
  -- ^ Total number of jobs for this branch
  , badgeState :: Maybe BadgeState
  -- ^ Badge state computed from job/commit comparison
  }
  deriving stock (Generic, Show, Eq)

{- | Get the most recent activity time for a branch.

Activity is defined as max(head commit date, latest job created time).
This ensures branches with recent commits OR recent builds appear first.
-}
branchActivityTime :: BranchDetails -> UTCTime
branchActivityTime details = case details.mLatestJob of
  Nothing -> details.branch.headCommit.date
  Just job -> max details.branch.headCommit.date job.jobCreatedTime

-- | Sorts branches by most recent activity descending (most recent first).
instance Ord BranchDetails where
  compare a b = compare (Down $ branchActivityTime a) (Down $ branchActivityTime b)

newtype JobId = JobId {unJobId :: Natural}
  deriving stock (Generic, Data)
  deriving newtype
    ( Show
    , Eq
    , Ord
    , Num
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    )

data Job = Job
  { repo :: RepoName
  -- ^ The name of the repository this job belongs to
  , branch :: BranchName
  -- ^ The name of the branch this job is running on
  , commit :: CommitID
  -- ^ The commit this job is running on
  , jobId :: JobId
  -- ^ The unique identifier of the job
  , jobWorkingDir :: FilePath
  -- ^ The working directory of the job
  , jobStatus :: JobStatus
  -- ^ The status of the job
  , jobCreatedTime :: UTCTime
  -- ^ When the job was created
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

type JobIxs = '[RepoName, BranchName, CommitID, JobId, JobStatus]
type IxJob = IxSet JobIxs Job

instance Indexable JobIxs Job where
  indices =
    ixList
      (ixFun $ \Job {repo} -> [repo])
      (ixFun $ \Job {branch} -> [branch])
      (ixFun $ \Job {commit} -> [commit])
      (ixFun $ \Job {jobId} -> [jobId])
      (ixFun $ \Job {jobStatus} -> [jobStatus])

data JobStatus
  = JobPending
  | JobRunning
  | JobFinished JobResult UTCTime
  | JobStale
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

data JobResult = JobSuccess | JobFailure | JobKilled
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- | Check if a job is currently active (pending or running)
jobIsActive :: Job -> Bool
jobIsActive job = case job.jobStatus of
  JobPending -> True
  JobRunning -> True
  JobFinished _ _ -> False
  JobStale -> False

-- | Get the end time for finished jobs only
jobEndTime :: Job -> Maybe UTCTime
jobEndTime job = case job.jobStatus of
  JobFinished _ endTime -> Just endTime
  _ -> Nothing

{- | Application state persisted to disk through acid-state

All operations (`query` or `update`) on this state are defined in Vira.State.Acid.
They can be invoked as follows:

>>> Just repo <- Vira.App.query $ GetRepoByNameA "my-repo"

Data in this state is indexed by `IxSet` to allow for efficient querying.
-}
data ViraState = ViraState
  { repos :: IxRepo
  , branches :: IxBranch
  , commits :: IxCommit
  , jobs :: IxJob
  }
  deriving stock (Generic, Typeable)

$(deriveSafeCopy 0 'base ''JobResult)
$(deriveSafeCopy 0 'base ''JobStatus)
$(deriveSafeCopy 0 'base ''JobId)
$(deriveSafeCopy 0 'base ''Job)
$(deriveSafeCopy 1 'base ''Branch)
$(deriveSafeCopy 0 'base ''BadgeState)
$(deriveSafeCopy 0 'base ''BranchDetails)
$(deriveSafeCopy 0 'base ''Repo)

{- | IMPORTANT: Increment the version number when making breaking changes to ViraState or its indexed types.
The version is automatically used by the --auto-reset-state feature to detect schema changes.
When enabled, auto-reset will remove ViraState/ and workspace/*/jobs directories on mismatch.
Run `vira info` to see the current schema version.
-}
$(deriveSafeCopy 4 'base ''ViraState)
