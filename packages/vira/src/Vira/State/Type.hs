{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.State.Type where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.IxSet.Typed
import Data.SafeCopy
import Servant.API (FromHttpApiData, ToHttpApiData)
import Vira.Lib.Attic
import Vira.Lib.Git (BranchName, CommitID)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

newtype RepoSettings = RepoSettings
  { dummy :: Maybe Text
  -- ^ Placeholder for future per-repo settings)
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)
  deriving anyclass (FromForm)

data AtticSettings = AtticSettings
  { atticServer :: AtticServer
  -- ^ Attic server information
  , atticCacheName :: AtticCache
  -- ^ Name of the attic cache
  , atticToken :: AtticToken
  -- ^ Access token for `atticServerUrl`
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- TODO: type-check field names during compile-time
instance FromForm AtticSettings where
  fromForm f =
    AtticSettings
      <$> fromForm f
      <*> parseUnique "atticCacheName" f
      <*> parseUnique "atticToken" f

data CachixSettings = CachixSettings
  { cachixName :: Text
  -- ^ Name of the cachix cache
  , authToken :: Text
  -- ^ Auth token for the cachix cache
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, ToJSON, FromJSON)

newtype RepoName = RepoName {unRepoName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToString
    , ToText
    , ToHttpApiData
    , FromHttpApiData
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )
  deriving anyclass (FromForm)

-- | A project's git repository
data Repo = Repo
  { name :: RepoName
  -- ^ An unique name identifying this repository
  , cloneUrl :: Text
  -- ^ The git clone URL of the repository
  , settings :: RepoSettings
  -- ^ repo-specific settings
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- TODO: type-check field names during compile-time
instance FromForm Repo where
  fromForm f =
    Repo
      <$> parseUnique "name" f
      <*> parseUnique "cloneUrl" f
      <*> fromForm f

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

type BranchIxs = '[RepoName, BranchName]
type IxBranch = IxSet BranchIxs Branch

instance Indexable BranchIxs Branch where
  indices =
    ixList
      (ixFun $ \Branch {repoName} -> [repoName])
      (ixFun $ \Branch {branchName} -> [branchName])

newtype JobId = JobId {unJobId :: Int}
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
  { jobRepo :: RepoName
  -- ^ The name of the repository this job belongs to
  , jobBranch :: BranchName
  -- ^ The name of the branch this job is running on
  , jobCommit :: CommitID
  -- ^ The commit this job is running on
  , jobId :: JobId
  -- ^ The unique identifier of the job
  , jobWorkingDir :: FilePath
  -- ^ The working directory of the job
  , jobStatus :: JobStatus
  -- ^ The status of the job
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

type JobIxs = '[RepoName, BranchName, CommitID, JobId, JobStatus]
type IxJob = IxSet JobIxs Job

instance Indexable JobIxs Job where
  indices =
    ixList
      (ixFun $ \Job {jobRepo} -> [jobRepo])
      (ixFun $ \Job {jobBranch} -> [jobBranch])
      (ixFun $ \Job {jobCommit} -> [jobCommit])
      (ixFun $ \Job {jobId} -> [jobId])
      (ixFun $ \Job {jobStatus} -> [jobStatus])

data JobStatus
  = JobPending
  | JobRunning
  | JobFinished JobResult
  | JobKilled
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

data JobResult = JobSuccess | JobFailure
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

$(deriveSafeCopy 0 'base ''JobResult)
$(deriveSafeCopy 0 'base ''JobStatus)
$(deriveSafeCopy 0 'base ''RepoName)
$(deriveSafeCopy 0 'base ''JobId)
$(deriveSafeCopy 0 'base ''Job)
$(deriveSafeCopy 0 'base ''Branch)
$(deriveSafeCopy 0 'base ''RepoSettings)
$(deriveSafeCopy 0 'base ''Repo)
$(deriveSafeCopy 0 'base ''CachixSettings)
$(deriveSafeCopy 0 'base ''AtticSettings)
