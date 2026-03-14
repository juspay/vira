{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.State.Type where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.IxSet.Typed
import Data.SafeCopy
import Data.Time (UTCTime)
import Effectful.Git (BranchName (..), Commit (..), CommitID, IxCommit, RepoName (..))
import Servant.API (FromHttpApiData, ToHttpApiData)
import Vira.Refresh.Type (RefreshResult)
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

-- * Pull Request types

-- | Pull request lifecycle state
data PRState = PROpen | PRClosed | PRMerged
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- | A pull request tracked by Vira
data PullRequest = PullRequest
  { repo :: RepoName
  -- ^ Repository this PR targets
  , prNumber :: Int
  -- ^ PR number (unique per repo)
  , title :: Text
  -- ^ PR title
  , headOwner :: OwnerName
  -- ^ Owner of the head (source) repo
  , baseOwner :: OwnerName
  -- ^ Owner of the base (target) repo
  , headBranch :: BranchName
  -- ^ Source branch name
  , baseBranch :: BranchName
  -- ^ Target branch in origin
  , prState :: PRState
  -- ^ Current lifecycle state
  , url :: Maybe Text
  -- ^ External URL to the PR on the forge (e.g. GitHub)
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

-- | Whether a PR is from a fork (head owner differs from base owner)
prIsFork :: PullRequest -> Bool
prIsFork pr = pr.headOwner /= pr.baseOwner

type PullRequestIxs = '[RepoName, Int]
type IxPullRequest = IxSet PullRequestIxs PullRequest

instance Indexable PullRequestIxs PullRequest where
  indices =
    ixList
      (ixFun $ \PullRequest {repo} -> [repo])
      (ixFun $ \PullRequest {prNumber} -> [prNumber])

-- | A commit pushed to a PR (tracks history of syncs)
data PRCommit = PRCommit
  { repo :: RepoName
  -- ^ Repository this commit belongs to
  , prNumber :: Int
  -- ^ PR number
  , commit :: Commit
  -- ^ The commit (id, message, date, author)
  , approved :: Bool
  -- ^ Fork PRs require approval; same-repo PRs are always True
  }
  deriving stock (Generic, Show, Typeable, Data, Eq, Ord)

type PRCommitIxs = '[RepoName, Int, CommitID]
type IxPRCommit = IxSet PRCommitIxs PRCommit

instance Indexable PRCommitIxs PRCommit where
  indices =
    ixList
      (ixFun $ \PRCommit {repo} -> [repo])
      (ixFun $ \PRCommit {prNumber} -> [prNumber])
      (ixFun $ \PRCommit {commit} -> [commit.id])

-- | Ref branch for PR jobs in the jobs index
prBranchRef :: Int -> BranchName
prBranchRef n = BranchName $ "refs/pull/" <> show n <> "/head"

-- | A project's git repository
data Repo = Repo
  { name :: RepoName
  -- ^ A unique name identifying this repository
  , cloneUrl :: Text
  -- ^ The git clone URL of the repository
  , lastRefresh :: Maybe RefreshResult
  -- ^ Metadata about the last 'Vira.Refresh.Daemon.refreshRepo' operation (persisted across restarts)
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

-- | Build freshness indicator for branches that have been built
data BuildFreshness
  = -- | Latest job commit matches head commit
    UpToDate
  | -- | Latest job commit differs from head commit
    OutOfDate
  deriving stock (Generic, Show, Eq)

-- | Build state for a 'Branch'
data BranchBuildState
  = -- | Branch has never been built
    NeverBuilt
  | -- | Branch has builds, with the latest job and freshness indicator
    Built Job BuildFreshness
  deriving stock (Generic, Show, Eq)

-- | Query parameters for filtering branches
data BranchQuery = BranchQuery
  { repoName :: Maybe RepoName
  -- ^ Filter by specific repository (Nothing = all repos)
  , branchNamePattern :: Maybe Text
  -- ^ Filter by branch name substring (Nothing = no name filter)
  , neverBuilt :: Maybe Bool
  -- ^ Nothing = all branches, Just True = unbuilt only, Just False = built only
  }
  deriving stock (Generic, Show, Eq)

instance Default BranchQuery where
  def = BranchQuery {repoName = Nothing, branchNamePattern = Nothing, neverBuilt = Nothing}

-- | 'Branch' with enriched metadata for display
data BranchDetails = BranchDetails
  { branch :: Branch
  -- ^ The 'Branch' information from the database
  , jobsCount :: Natural
  -- ^ Total number of 'Job's for this branch
  , buildState :: BranchBuildState
  -- ^ Build state computed from job/commit comparison (includes latest job if built)
  }
  deriving stock (Generic, Show, Eq)

{- | Get the most recent activity time for a 'BranchDetails'.

Activity is defined as @max(head commit date, latest job created time)@.
This ensures branches with recent commits OR recent builds appear first.
-}
branchActivityTime :: BranchDetails -> UTCTime
branchActivityTime details = case details.buildState of
  NeverBuilt -> details.branch.headCommit.date
  Built job _ -> max details.branch.headCommit.date job.jobCreatedTime

-- | Sorts 'BranchDetails' by most recent activity descending (most recent first).
instance Ord BranchDetails where
  compare a b = compare (Down $ branchActivityTime a) (Down $ branchActivityTime b)

-- | Build/approval state for a PR (mirrors 'BranchBuildState')
data PRBuildState
  = -- | Fork PR with latest unapproved commit
    PRUnapproved PRCommit
  | -- | All approved but no job yet
    PRNeverBuilt
  | -- | Has at least one build (latest job + freshness)
    PRBuilt Job BuildFreshness
  deriving stock (Generic, Show, Eq)

-- | 'PullRequest' enriched with build state for display (mirrors 'BranchDetails')
data PRDetails = PRDetails
  { pullRequest :: PullRequest
  , latestCommitTime :: UTCTime
  -- ^ Time of the most recent PR commit (analogous to @branch.headCommit.date@)
  , buildState :: PRBuildState
  }
  deriving stock (Generic, Show, Eq)

{- | Get the most recent activity time for a 'PRDetails'.

Uses @max(latestCommitTime, jobCreatedTime)@, mirroring 'branchActivityTime'.
-}
prActivityTime :: PRDetails -> UTCTime
prActivityTime details = case details.buildState of
  PRUnapproved _ -> details.latestCommitTime
  PRNeverBuilt -> details.latestCommitTime
  PRBuilt job _ -> max details.latestCommitTime job.jobCreatedTime

-- | Sorts 'PRDetails' by most recent activity descending (most recent first).
instance Ord PRDetails where
  compare a b = compare (Down $ prActivityTime a) (Down $ prActivityTime b)

newtype OwnerName = OwnerName {unOwnerName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord, IsString, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

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
  -- ^ For branch jobs: branch name; For PR jobs: @refs\/pull\/:n\/head@
  , commit :: CommitID
  -- ^ The commit this job is running on
  , prNumber :: Maybe Int
  -- ^ Just for PR jobs, Nothing for branch jobs
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

-- | Check if a 'Job' is currently active (pending or running)
jobIsActive :: Job -> Bool
jobIsActive job = case job.jobStatus of
  JobPending -> True
  JobRunning -> True
  JobFinished _ _ -> False
  JobStale -> False

-- | Get the end time for finished 'Job's only
jobEndTime :: Job -> Maybe UTCTime
jobEndTime job = case job.jobStatus of
  JobFinished _ endTime -> Just endTime
  _ -> Nothing

{- | Application state persisted to disk through acid-state

All operations (@query@ or @update@) on this state are defined in 'Vira.State.Acid'.
They can be invoked as follows:

>>> Just repo <- Vira.App.query $ GetRepoByNameA "my-repo"

Data in this state is indexed by 'Data.IxSet.Typed.IxSet' to allow for efficient querying.
-}
data ViraState = ViraState
  { repos :: IxRepo
  , branches :: IxBranch
  , commits :: IxCommit
  , jobs :: IxJob
  , pullRequests :: IxPullRequest
  , prCommits :: IxPRCommit
  , nextJobId :: JobId
  -- ^ The next job ID to assign (monotonically increasing)
  }
  deriving stock (Generic, Typeable)

$(deriveSafeCopy 0 'base ''OwnerName)
$(deriveSafeCopy 0 'base ''PRState)
$(deriveSafeCopy 0 'base ''PullRequest)
$(deriveSafeCopy 0 'base ''PRCommit)
$(deriveSafeCopy 0 'base ''JobResult)
$(deriveSafeCopy 0 'base ''JobStatus)
$(deriveSafeCopy 0 'base ''JobId)
$(deriveSafeCopy 0 'base ''Job)
$(deriveSafeCopy 1 'base ''Branch)
$(deriveSafeCopy 0 'base ''BuildFreshness)
$(deriveSafeCopy 0 'base ''BranchBuildState)
$(deriveSafeCopy 0 'base ''BranchQuery)
$(deriveSafeCopy 0 'base ''BranchDetails)
$(deriveSafeCopy 0 'base ''PRBuildState)
$(deriveSafeCopy 0 'base ''PRDetails)
$(deriveSafeCopy 0 'base ''Repo)

{- | IMPORTANT: Increment the version number when making breaking changes to 'ViraState' or its indexed types.
The version is automatically used by the @--auto-reset-state@ feature to detect schema changes.
When enabled, auto-reset will remove @ViraState/@ and @workspace/*/jobs@ directories on mismatch.
Run @vira info@ to see the current schema version.
-}
$(deriveSafeCopy 9 'base ''ViraState)
