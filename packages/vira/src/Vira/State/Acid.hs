{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

-- | acid-state implementation for Vira state
module Vira.State.Acid where

import Data.Acid (Query, Update, makeAcidic)
import Data.IxSet.Typed
import Data.IxSet.Typed qualified as Ix
import Data.List (maximum)
import Data.Map.Strict qualified as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)
import Effectful.Git (BranchName, Commit (..), CommitID, IxCommit, RepoName)
import System.FilePath ((</>))
import Vira.State.Type

{- | Application that gets persisted to disk through acid-state

All operations (`query` or `update`) on this state are defined immediately below. They can be invoked as follows:

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

{- | IMPORTANT: Increment the version number when making breaking changes to ViraState or its indexed types.
The version is automatically used by the --auto-reset-db feature to detect schema changes.
Run `vira info` to see the current schema version.
-}
$(deriveSafeCopy 1 'base ''ViraState)

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

-- | Get all branches of a repository
getBranchesByRepoA :: RepoName -> Query ViraState [Branch]
getBranchesByRepoA name = do
  ViraState {branches} <- ask
  pure $ Ix.toList $ branches @= name

-- | Get a repo's branch by name
getBranchByNameA :: RepoName -> BranchName -> Query ViraState (Maybe Branch)
getBranchByNameA repo branch = do
  ViraState {branches} <- ask
  pure $ Ix.getOne $ branches @= repo @= branch

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
      repoBranches = Map.toList branches <&> \(branchName, commit) -> Branch repo branchName commit.id
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

-- | Get all jobs of a repository (across all branches) in descending order by JobId
getJobsByRepoA :: RepoName -> Query ViraState [Job]
getJobsByRepoA repo = do
  ViraState {jobs} <- ask
  pure $ Ix.toDescList (Proxy @JobId) $ jobs @= repo

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
    , 'getBranchesByRepoA
    , 'getBranchByNameA
    , 'setRepoA
    , 'setRepoBranchesA
    , 'getCommitByIdA
    , 'storeCommitA
    , 'getJobsByBranchA
    , 'getJobsByRepoA
    , 'getRunningJobs
    , 'getJobA
    , 'addNewJobA
    , 'jobUpdateStatusA
    , 'markUnfinishedJobsAsStaleA
    , 'addNewRepoA
    , 'deleteRepoByNameA
    ]
 )
