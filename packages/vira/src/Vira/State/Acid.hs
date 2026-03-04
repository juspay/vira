{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | acid-state implementation for Vira state
module Vira.State.Acid where

import Data.Acid (Query, Update, makeAcidic)
import Data.IxSet.Typed
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.SafeCopy
import Data.Text qualified as T
import Data.Time (UTCTime)
import Effectful.Git (BranchName, Commit (..), CommitID, RepoName)
import System.FilePath ((</>))
import Vira.Refresh.Type (RefreshResult)
import Vira.State.Type

-- | Set all repositories, replacing existing ones
setAllReposA :: [Repo] -> Update ViraState ()
setAllReposA repos = do
  modify $ \s ->
    s
      { repos = Ix.fromList repos
      }

-- | Add a new 'Repo'
addNewRepoA :: Repo -> Update ViraState ()
addNewRepoA repo = do
  modify $ \s ->
    s
      { repos = Ix.insert repo s.repos
      }

{- | Delete a 'Repo' by name and all associated data ('Branch'es and 'Job's)

Returns @Left@ with error message if there are running jobs
-}
deleteRepoByNameA :: RepoName -> Update ViraState (Either Text ())
deleteRepoByNameA name = do
  s <- get
  -- Check for running jobs
  let repoJobs = Ix.toList $ s.jobs @= name
      runningJobs = filter jobIsActive repoJobs
  if not (Prelude.null runningJobs)
    then pure $ Left "Cannot delete repository with running jobs. Please wait for jobs to finish or kill them first."
    else do
      modify $ \st ->
        st
          { repos = Ix.deleteIx name st.repos
          , branches = deleteIxMulti name st.branches
          , jobs = deleteIxMulti name st.jobs
          }
      pure $ Right ()

-- | Get all 'Repo's
getAllReposA :: Query ViraState [Repo]
getAllReposA = do
  ViraState {repos} <- ask
  pure $ Ix.toList repos

-- | Get a 'Repo' by 'RepoName'
getRepoByNameA :: RepoName -> Query ViraState (Maybe Repo)
getRepoByNameA name = do
  ViraState {repos} <- ask
  pure $ Ix.getOne $ repos @= name

-- | Enrich a 'Branch' with its 'Job' metadata to create 'BranchDetails'
enrichBranchWithJobs :: IxJob -> Branch -> BranchDetails
enrichBranchWithJobs jobsIx branch =
  let branchJobs = Ix.toDescList (Proxy @JobId) $ jobsIx @= branch.repoName @= branch.branchName
      -- Compute build state based on job and commit comparison
      buildState = case viaNonEmpty head branchJobs of
        Nothing -> NeverBuilt
        Just job
          | job.commit /= branch.headCommit.id -> Built job OutOfDate
          | otherwise -> Built job UpToDate
   in BranchDetails
        { branch
        , jobsCount = fromIntegral $ length branchJobs
        , buildState
        }

{- | Query branches with enriched metadata, filtered by BranchQuery criteria.

This is the canonical query for getting branches - used by both RepoPage and IndexPage.

- Nothing repo: all repos (IndexPage)
- Just repo: single repo (RepoPage)
- Filter by branch name if provided
- Filter by build status (Nothing = all, Just True = unbuilt only, Just False = built only)
- Sorted by activity time (most recent first)
-}
queryBranchDetailsA :: BranchQuery -> Natural -> Query ViraState [BranchDetails]
queryBranchDetailsA query limit = do
  ViraState {branches, jobs} <- ask
  pure $
    branches
      & maybe Prelude.id getEQ query.repoName
      & Ix.toList
      & filter matchesBranch
      & fmap (enrichBranchWithJobs jobs)
      & filterByBuildStatus
      & sortWith (Down . branchActivityTime)
      & take (fromIntegral limit)
  where
    matchesBranch branch = case query.branchNamePattern of
      Nothing -> True
      Just q -> T.toLower q `T.isInfixOf` T.toLower (toText branch.branchName)
    filterByBuildStatus = case query.neverBuilt of
      Nothing -> Prelude.id -- all branches
      Just True -> filter (\d -> d.buildState == NeverBuilt)
      Just False -> filter (\d -> d.buildState /= NeverBuilt)

-- | Get single branch with enriched metadata
getBranchDetailsA :: RepoName -> BranchName -> Query ViraState (Maybe BranchDetails)
getBranchDetailsA repo branchName = do
  ViraState {branches, jobs} <- ask
  pure $ do
    branch <- Ix.getOne $ branches @= repo @= branchName
    pure $ enrichBranchWithJobs jobs branch

-- | Get all branches for a repo
getRepoBranchesA :: RepoName -> Query ViraState [Branch]
getRepoBranchesA repo = do
  ViraState {branches} <- ask
  pure $ Ix.toList $ branches @= repo

-- | Get a repo's branch by name
getBranchByNameA :: RepoName -> BranchName -> Query ViraState (Maybe Branch)
getBranchByNameA repo branch = do
  ViraState {branches} <- ask
  pure $ Ix.getOne $ branches @= repo @= branch

-- | Set a repository's refresh status
setRefreshStatusA :: RepoName -> Maybe RefreshResult -> Update ViraState ()
setRefreshStatusA name mResult = do
  modify $ \s ->
    case Ix.getOne $ s.repos @= name of
      Nothing -> s -- Repo doesn't exist, no-op
      Just repo ->
        let updatedRepo = repo {lastRefresh = mResult}
         in s {repos = Ix.updateIx name updatedRepo s.repos}

-- | Represents a branch update with old and new commit
data BranchUpdate = BranchUpdate
  { branch :: BranchName
  -- ^ The branch name
  , oldCommit :: Maybe Commit
  -- ^ Nothing if new branch
  , newCommit :: Commit
  -- ^ The new commit for the branch that this update fetched
  , neverBuilt :: Bool
  -- ^ Whether this branch was attempted to be built at least once.
  }
  deriving stock (Show, Eq, Generic)

{- | Set a repository's branches, marking deleted branches (keeps jobs for history)

Returns list of branch updates (branches with new commits only)
-}
setRepoBranchesA :: RepoName -> Map BranchName Commit -> Update ViraState [BranchUpdate]
setRepoBranchesA repoName branches = state $ \s ->
  let
    mkBranchUpdate branch newCommit = do
      let oldBranch = Ix.getOne (s.branches @= repoName @= branch)
          oldCommit = (.headCommit) <$> oldBranch
          neverBuilt = Ix.null $ s.jobs @= repoName @= branch
      -- skip if commit is unchanged
      guard $ fmap (.id) oldCommit /= Just newCommit.id
      pure $ BranchUpdate {..}

    -- Compute updates (new or changed commits)
    updates = uncurry mkBranchUpdate `mapMaybe` Map.toList branches

    -- Old branches that are no longer on remote - mark as deleted
    deletedBranches =
      filter (\b -> b.branchName `notElem` Map.keys branches) (Ix.toList $ s.branches @= repoName)
        <&> \b -> b {deleted = True}

    -- Branches from remote - create with deleted=False
    activeBranches =
      Map.toList branches <&> \(branchName, headCommit) ->
        Branch repoName branchName headCommit False

    -- Combine deleted + active branches
    allBranches = deletedBranches <> activeBranches
    commits = Map.elems branches
   in
    ( updates
    , s
        { branches = updateIxMulti repoName (Ix.fromList allBranches) s.branches
        , commits = s.commits ||| Ix.fromList commits
        }
    )

-- | Get a commit by its ID
getCommitByIdA :: CommitID -> Query ViraState (Maybe Commit)
getCommitByIdA commitId = do
  ViraState {commits} <- ask
  pure $ Ix.getOne $ commits @= commitId

-- | Store a commit in the index
storeCommitA :: Commit -> Update ViraState ()
storeCommitA commit = do
  modify $ \s ->
    s
      { commits = Ix.updateIx commit.id commit s.commits
      }

-- | Get all jobs of a repo's branch in descending order
getJobsByBranchA :: RepoName -> BranchName -> Query ViraState [Job]
getJobsByBranchA repo branch = do
  ViraState {jobs} <- ask
  pure $ Ix.toDescList (Proxy @JobId) $ jobs @= repo @= branch

-- | Get the N most recent jobs across all repos/branches, showing only latest job per repo/branch
getRecentJobsA :: Natural -> Query ViraState [Job]
getRecentJobsA limit = do
  ViraState {jobs} <- ask
  let allJobs = sortWith (Down . (.jobCreatedTime)) $ Ix.toList jobs
      -- Keep first occurrence of each (repo, branch) - already sorted newest-first
      latestPerBranch = ordNubOn (\job -> (job.repo, job.branch)) allJobs
  pure $ take (fromIntegral limit) latestPerBranch

-- | Active jobs grouped by status
data ActiveJobs = ActiveJobs
  { running :: [Job]
  , pending :: [Job]
  }
  deriving stock (Show, Generic)
  deriving anyclass (SafeCopy)

-- | Get all active jobs (running + pending) in single query
getActiveJobsA :: Query ViraState ActiveJobs
getActiveJobsA = do
  ViraState {jobs} <- ask
  pure $
    ActiveJobs
      { running = Ix.toList $ jobs @= JobRunning
      , pending = Ix.toList $ jobs @= JobPending
      }

getJobA :: JobId -> Query ViraState (Maybe Job)
getJobA jobId = do
  ViraState {jobs} <- ask
  pure $ Ix.getOne $ jobs @= jobId

-- | Get all finished jobs older than the cutoff time (returns job ID, end time, and workspace directory)
getOldJobsA :: UTCTime -> Query ViraState [(JobId, UTCTime, FilePath)]
getOldJobsA cutoffTime = do
  ViraState {jobs} <- ask
  let allJobs = Ix.toList jobs
      -- Only finished jobs have an end time
      oldJobIds = mapMaybe extractOldJobId allJobs
  pure $ sortWith (\(_, endTime, _) -> endTime) oldJobIds -- Oldest first
  where
    extractOldJobId job = case jobEndTime job of
      Nothing -> Nothing -- Not finished (Pending/Running/Stale)
      Just endTime
        | endTime < cutoffTime -> Just (job.jobId, endTime, job.jobWorkingDir)
        | otherwise -> Nothing

-- | Create a new job returning it.
addNewJobA :: RepoName -> BranchName -> CommitID -> Maybe Int -> FilePath -> UTCTime -> Update ViraState Job
addNewJobA repo branch commit prNumber baseDir jobCreatedTime = state $ \s ->
  let
    jobId = s.nextJobId
    jobStatus = JobPending
    jobWorkingDir = baseDir </> show jobId
    job = Job {..}
   in
    (job, s {jobs = Ix.insert job s.jobs, nextJobId = s.nextJobId + 1})

jobUpdateStatusA :: JobId -> JobStatus -> Update ViraState Job
jobUpdateStatusA jobId status = state $ \s ->
  let job = fromMaybe (error $ "No such job: " <> show jobId) $ Ix.getOne $ s.jobs @= jobId
      updatedJob = job {jobStatus = status}
   in (updatedJob, s {jobs = Ix.updateIx jobId updatedJob s.jobs})

-- | Delete a job from the state (for cleanup of old jobs)
deleteJobA :: JobId -> Update ViraState ()
deleteJobA jobId = do
  modify $ \s -> s {jobs = Ix.deleteIx jobId s.jobs}

markUnfinishedJobsAsStaleA :: Update ViraState ()
markUnfinishedJobsAsStaleA = do
  jobs <- Ix.toList <$> gets jobs
  forM_ jobs $ \job -> do
    when (jobIsActive job) $ do
      void $ jobUpdateStatusA job.jobId JobStale

{- | Cancel all pending jobs for a (repo, branch) pair
Returns the number of jobs cancelled
-}
cancelPendingJobsInBranchA :: RepoName -> BranchName -> UTCTime -> Update ViraState Natural
cancelPendingJobsInBranchA repo branch endTime = state $ \s ->
  let jobs = Ix.toList $ s.jobs @= repo @= branch
      pendingJobs = filter (\j -> j.jobStatus == JobPending) jobs
      updatedState = flipfoldl' (\job st -> st {jobs = Ix.updateIx job.jobId (job {jobStatus = JobFinished JobKilled endTime}) st.jobs}) s pendingJobs
   in (fromIntegral $ length pendingJobs, updatedState)

-- * PullRequest operations

-- | Get a pull request by repo and PR number
getPullRequestA :: RepoName -> Int -> Query ViraState (Maybe PullRequest)
getPullRequestA repo prNum = do
  ViraState {pullRequests} <- ask
  pure $ Ix.getOne $ pullRequests @= repo @= prNum

-- | Get all pull requests for a repo
getPullRequestsByRepoA :: RepoName -> Query ViraState [PullRequest]
getPullRequestsByRepoA repo = do
  ViraState {pullRequests} <- ask
  pure $ Ix.toList $ pullRequests @= repo

-- | Upsert a pull request (create or update)
upsertPullRequestA :: PullRequest -> Update ViraState ()
upsertPullRequestA pr = do
  modify $ \s ->
    s {pullRequests = Ix.updateIx pr.prNumber pr s.pullRequests}

-- | Update a pull request's state (on close/merge)
updatePullRequestStateA :: RepoName -> Int -> PRState -> Update ViraState ()
updatePullRequestStateA repo prNum newState = do
  modify $ \s ->
    case Ix.getOne $ s.pullRequests @= repo @= prNum of
      Nothing -> s
      Just pr ->
        let updated = pr {prState = newState}
         in s {pullRequests = Ix.updateIx prNum updated s.pullRequests}

-- * PRCommit operations

-- | Get a specific PR commit
getPRCommitA :: RepoName -> Int -> CommitID -> Query ViraState (Maybe PRCommit)
getPRCommitA repo prNum sha = do
  ViraState {prCommits} <- ask
  pure $ Ix.getOne $ prCommits @= repo @= prNum @= sha

-- | Get all commits for a PR
getPRCommitsByPRA :: RepoName -> Int -> Query ViraState [PRCommit]
getPRCommitsByPRA repo prNum = do
  ViraState {prCommits} <- ask
  pure $ Ix.toList $ prCommits @= repo @= prNum

-- | Get unapproved commits for a PR
getUnapprovedPRCommitsA :: RepoName -> Int -> Query ViraState [PRCommit]
getUnapprovedPRCommitsA repo prNum = do
  ViraState {prCommits} <- ask
  pure $ filter (not . (.approved)) $ Ix.toList $ prCommits @= repo @= prNum

-- | Add a new PR commit (upserts by SHA to avoid duplicates from repeated webhook events)
addPRCommitA :: PRCommit -> Update ViraState ()
addPRCommitA pc = do
  modify $ \s ->
    s {prCommits = Ix.updateIx pc.sha pc s.prCommits}

-- | Approve a PR commit, returning Left if not found
approvePRCommitA :: RepoName -> Int -> CommitID -> Update ViraState (Either Text ())
approvePRCommitA repo prNum sha = do
  s <- get
  case Ix.getOne $ s.prCommits @= repo @= prNum @= sha of
    Nothing -> pure $ Left "PR commit not found"
    Just pc -> do
      let updated = pc {approved = True}
      modify $ \st -> st {prCommits = Ix.updateIx sha updated st.prCommits}
      pure $ Right ()

-- | Like `Ix.updateIx`, but works for multiple items.
updateIxMulti ::
  (Ix.IsIndexOf ix ixs, Ix.Indexable ixs a) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
updateIxMulti r new rels =
  let old = rels @= r
      deleteMany = foldr Ix.delete
   in new `Ix.union` (rels `deleteMany` old)

-- | Like `Ix.deleteIx`, but works for multiple items
deleteIxMulti ::
  (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
deleteIxMulti r rels =
  let candidates = Ix.toList $ Ix.getEQ r rels
   in flipfoldl' Ix.delete rels candidates

$(deriveSafeCopy 1 'base ''BranchUpdate)

$( makeAcidic
     ''ViraState
     [ 'setAllReposA
     , 'getAllReposA
     , 'getRepoByNameA
     , 'queryBranchDetailsA
     , 'getRepoBranchesA
     , 'getBranchByNameA
     , 'getBranchDetailsA
     , 'setRefreshStatusA
     , 'setRepoBranchesA
     , 'getCommitByIdA
     , 'storeCommitA
     , 'getJobsByBranchA
     , 'getRecentJobsA
     , 'getActiveJobsA
     , 'getJobA
     , 'getOldJobsA
     , 'addNewJobA
     , 'jobUpdateStatusA
     , 'deleteJobA
     , 'markUnfinishedJobsAsStaleA
     , 'cancelPendingJobsInBranchA
     , 'addNewRepoA
     , 'deleteRepoByNameA
     , -- PullRequest operations
       'getPullRequestA
     , 'getPullRequestsByRepoA
     , 'upsertPullRequestA
     , 'updatePullRequestStateA
     , -- PRCommit operations
       'getPRCommitA
     , 'getPRCommitsByPRA
     , 'getUnapprovedPRCommitsA
     , 'addPRCommitA
     , 'approvePRCommitA
     ]
 )

-- * Show instances (for events published via App.update to event bus)

deriving stock instance Show AddNewRepoA

deriving stock instance Show DeleteRepoByNameA

deriving stock instance Show SetRefreshStatusA

deriving stock instance Show SetRepoBranchesA

deriving stock instance Show StoreCommitA

deriving stock instance Show AddNewJobA

deriving stock instance Show JobUpdateStatusA

deriving stock instance Show DeleteJobA

deriving stock instance Show CancelPendingJobsInBranchA

deriving stock instance Show UpsertPullRequestA

deriving stock instance Show UpdatePullRequestStateA

deriving stock instance Show AddPRCommitA

deriving stock instance Show ApprovePRCommitA
