{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.State.Type where

import Data.Data (Data)
import Data.IxSet.Typed
import Data.SafeCopy
import Data.Time (UTCTime)
import Effectful.Git (BranchName, CommitID, RepoName (..))
import Servant.API (FromHttpApiData, ToHttpApiData)
import Vira.Refresh.Type (RefreshStatus (..))
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

-- | A project's git repository
data Repo = Repo
  { name :: RepoName
  -- ^ An unique name identifying this repository
  , cloneUrl :: Text
  -- ^ The git clone URL of the repository
  , lastRefreshTime :: Maybe UTCTime
  -- ^ When this repository was last refreshed
  , lastRefreshStatus :: RefreshStatus
  -- ^ Status of the last refresh attempt
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- TODO: type-check field names during compile-time
instance FromForm Repo where
  fromForm f =
    Repo
      <$> parseUnique "name" f
      <*> parseUnique "cloneUrl" f
      <*> pure Nothing -- lastRefreshTime
      <*> pure RefreshPending -- lastRefreshStatus

type RepoIxs = '[RepoName]
type IxRepo = IxSet RepoIxs Repo

instance Indexable RepoIxs Repo where
  indices = ixList (ixFun $ \Repo {name} -> [name])

data Branch = Branch
  { repoName :: RepoName
  -- ^ The name of the repository this branch belongs to
  , branchName :: BranchName
  -- ^ The name of the branch
  , headCommit :: CommitID
  -- ^ The commit at the head of the branch
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

type BranchIxs = '[RepoName, BranchName, CommitID]
type IxBranch = IxSet BranchIxs Branch

instance Indexable BranchIxs Branch where
  indices =
    ixList
      (ixFun $ \Branch {repoName} -> [repoName])
      (ixFun $ \Branch {branchName} -> [branchName])
      (ixFun $ \Branch {headCommit} -> [headCommit])

newtype JobId = JobId {unJobId :: Natural}
  deriving stock (Generic, Data)
  deriving newtype
    ( Show
    , Eq
    , Ord
    , Num
    , ToHttpApiData
    , FromHttpApiData
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

$(deriveSafeCopy 0 'base ''JobResult)
$(deriveSafeCopy 0 'base ''JobStatus)
$(deriveSafeCopy 0 'base ''JobId)
$(deriveSafeCopy 0 'base ''Job)
$(deriveSafeCopy 0 'base ''Branch)
$(deriveSafeCopy 0 'base ''Repo)
