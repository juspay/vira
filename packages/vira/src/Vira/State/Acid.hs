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
import Data.List (maximum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Effectful.Git (BranchName, Commit (..), CommitID, RepoName)
import System.FilePath ((</>))
import Vira.State.Type

-- | Set all repositories, replacing existing ones
setAllReposA :: [Repo] -> Update ViraState ()
setAllReposA repos = do
  modify $ \s ->
    s
      { repos = Ix.fromList repos
      }

-- | Add a new repository
addNewRepoA :: Repo -> Update ViraState ()
addNewRepoA repo = do
  modify $ \s ->
    s
      { repos = Ix.insert repo s.repos
      }

-- | Delete a repository by name
deleteRepoByNameA :: RepoName -> Update ViraState ()
deleteRepoByNameA name = do
  modify $ \s ->
    s
      { repos = Ix.deleteIx name s.repos
      }

-- | Get all repositories
getAllReposA :: Query ViraState [Repo]
getAllReposA = do
  ViraState {repos} <- ask
  pure $ Ix.toList repos

-- | Get a repository by name
getRepoByNameA :: RepoName -> Query ViraState (Maybe Repo)
getRepoByNameA name = do
  ViraState {repos} <- ask
  pure $ Ix.getOne $ repos @= name

-- | Enrich a branch with its job metadata
enrichBranchWithJobs :: IxJob -> Branch -> BranchDetails
enrichBranchWithJobs jobsIx branch =
  let branchJobs = Ix.toDescList (Proxy @JobId) $ jobsIx @= branch.repoName @= branch.branchName
      mLatestJob = viaNonEmpty head branchJobs
      -- Compute badge state based on job and commit comparison
      badgeState = case mLatestJob of
        Nothing -> Just NeverBuilt
        Just job
          | job.commit /= branch.headCommit.id -> Just OutOfDate
          | otherwise -> Nothing
   in BranchDetails
        { branch
        , mLatestJob
        , jobsCount = fromIntegral $ length branchJobs
        , badgeState
        }

{- | Get branches with enriched metadata, optionally filtered by repo and/or name.

This is the canonical query for getting branches - used by both RepoPage and IndexPage.

- Nothing repo: all repos (IndexPage)
- Just repo: single repo (RepoPage)
- Filter by branch name if provided
- Sorted by activity time (most recent first)
-}
getAllBranchesA :: Maybe RepoName -> Maybe Text -> Natural -> Query ViraState [BranchDetails]
getAllBranchesA mRepo mFilter limit = do
  ViraState {branches, jobs} <- ask
  let candidateBranches = case mRepo of
        Nothing -> Ix.toList branches
        Just repo -> Ix.toList $ branches @= repo
      matchesFilter branch = case mFilter of
        Nothing -> True
        Just s -> T.toLower s `T.isInfixOf` T.toLower (toText branch.branchName)
      enriched = enrichBranchWithJobs jobs <$> filter matchesFilter candidateBranches
      sorted = sortWith (Down . branchActivityTime) enriched
  pure $ take (fromIntegral limit) sorted

-- | Get a repo's branch by name
getBranchByNameA :: RepoName -> BranchName -> Query ViraState (Maybe Branch)
getBranchByNameA repo branch = do
  ViraState {branches} <- ask
  pure $ Ix.getOne $ branches @= repo @= branch

-- | Get branch with enriched metadata
getBranchDetailsA :: RepoName -> BranchName -> Query ViraState (Maybe BranchDetails)
getBranchDetailsA repo branchName = do
  ViraState {branches, jobs} <- ask
  case Ix.getOne $ branches @= repo @= branchName of
    Nothing -> pure Nothing
    Just branch -> pure $ Just $ enrichBranchWithJobs jobs branch

-- | Set a repository
setRepoA :: Repo -> Update ViraState ()
setRepoA repo = do
  modify $ \s ->
    s
      { repos = Ix.updateIx repo.name repo s.repos
      }

-- | Set a repository's branches
setRepoBranchesA :: RepoName -> Map BranchName Commit -> Update ViraState ()
setRepoBranchesA repo branches = do
  modify $ \s ->
    let
      repoBranches = Map.toList branches <&> uncurry (Branch repo)
      commits = Map.elems branches
     in
      s
        { branches = updateIxMulti repo (Ix.fromList repoBranches) s.branches
        , commits = s.commits ||| Ix.fromList commits
        }

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

-- | Get all running jobs
getRunningJobs :: Query ViraState [Job]
getRunningJobs = do
  ViraState {jobs} <- ask
  let allJobs = Ix.toList jobs
  pure $ filter jobIsActive allJobs

getJobA :: JobId -> Query ViraState (Maybe Job)
getJobA jobId = do
  ViraState {jobs} <- ask
  pure $ Ix.getOne $ jobs @= jobId

-- | Create a new job returning it.
addNewJobA :: RepoName -> BranchName -> CommitID -> FilePath -> UTCTime -> Update ViraState Job
addNewJobA repo branch commit baseDir jobCreatedTime = do
  jobs <- Ix.toList <$> gets jobs
  let
    jobId =
      let ids = (.jobId) <$> jobs
       in if Prelude.null ids then JobId 1 else JobId 1 + maximum ids
    jobStatus = JobPending
    jobWorkingDir = baseDir </> show jobId
    job = Job {..}
  modify $ \s ->
    s
      { jobs = Ix.insert job s.jobs
      }
  pure job

jobUpdateStatusA :: JobId -> JobStatus -> Update ViraState ()
jobUpdateStatusA jobId status = do
  modify $ \s -> do
    let job = fromMaybe (error $ "No such job: " <> show jobId) $ Ix.getOne $ s.jobs @= jobId
    s
      { jobs = Ix.updateIx jobId (job {jobStatus = status}) s.jobs
      }

markUnfinishedJobsAsStaleA :: Update ViraState ()
markUnfinishedJobsAsStaleA = do
  jobs <- Ix.toList <$> gets jobs
  forM_ jobs $ \job -> do
    when (jobIsActive job) $ do
      jobUpdateStatusA job.jobId JobStale

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

$( makeAcidic
     ''ViraState
     [ 'setAllReposA
     , 'getAllReposA
     , 'getRepoByNameA
     , 'getAllBranchesA
     , 'getBranchByNameA
     , 'getBranchDetailsA
     , 'setRepoA
     , 'setRepoBranchesA
     , 'getCommitByIdA
     , 'storeCommitA
     , 'getJobsByBranchA
     , 'getRecentJobsA
     , 'getRunningJobs
     , 'getJobA
     , 'addNewJobA
     , 'jobUpdateStatusA
     , 'markUnfinishedJobsAsStaleA
     , 'addNewRepoA
     , 'deleteRepoByNameA
     ]
 )
