{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.State.Type where

import Data.Data (Data)
import Data.IxSet.Typed
import Data.SafeCopy
import Servant.API (FromHttpApiData, ToHttpApiData)
import Vira.Lib.Attic
import Vira.Lib.Git (BranchName, CommitID)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

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
  deriving anyclass (FromForm)

newtype RepoName = RepoName {unRepoName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToString
    , ToText
    , ToHttpApiData
    , FromHttpApiData
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

data Platform
  = PlatformLinux
  | PlatformMacOS
  | PlatformMacOSIntel
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

instance FromHttpApiData Platform where
  parseUrlPiece "linux" = Right PlatformLinux
  parseUrlPiece "macos" = Right PlatformMacOS
  parseUrlPiece "macos-intel" = Right PlatformMacOSIntel
  parseUrlPiece _ = Left "Invalid platform"

instance ToHttpApiData Platform where
  toUrlPiece PlatformLinux = "linux"
  toUrlPiece PlatformMacOS = "macos"
  toUrlPiece PlatformMacOSIntel = "macos-intel"

instance FromForm Platform where
  fromForm f = do
    platformStr <- parseUnique "platform" f
    case (platformStr :: Text) of
      "linux" -> Right PlatformLinux
      "macos" -> Right PlatformMacOS
      "macos-intel" -> Right PlatformMacOSIntel
      _ -> Left "Invalid platform"

newtype RemoteBuilderId = RemoteBuilderId {unRemoteBuilderId :: Int}
  deriving stock (Generic, Data)
  deriving newtype
    ( Show
    , Eq
    , Ord
    , Num
    , ToHttpApiData
    , FromHttpApiData
    )

data RemoteBuilder = RemoteBuilder
  { remoteBuilderUser :: Text
  -- ^ SSH username for the remote builder
  , remoteBuilderHost :: Text
  -- ^ SSH hostname for the remote builder
  , remoteBuilderPlatforms :: [Platform]
  -- ^ List of platforms this host supports
  , remoteBuilderNote :: Maybe Text
  -- ^ Optional note/description for this builder
  , remoteBuilderId :: RemoteBuilderId
  -- ^ Unique identifier for this remote builder
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- | Form data for adding/updating remote builders (without ID)
data RemoteBuilderForm = RemoteBuilderForm
  { remoteBuilderFormUser :: Text
  , remoteBuilderFormHost :: Text
  , remoteBuilderFormPlatforms :: [Platform]
  , remoteBuilderFormNote :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)

type RemoteBuilderIxs = '[RemoteBuilderId]
type IxRemoteBuilder = IxSet RemoteBuilderIxs RemoteBuilder

instance Indexable RemoteBuilderIxs RemoteBuilder where
  indices = ixList (ixFun $ \RemoteBuilder {remoteBuilderId} -> [remoteBuilderId])

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
$(deriveSafeCopy 0 'base ''Platform)
$(deriveSafeCopy 0 'base ''RemoteBuilderId)
$(deriveSafeCopy 0 'base ''RemoteBuilder)
$(deriveSafeCopy 0 'base ''RemoteBuilderForm)
