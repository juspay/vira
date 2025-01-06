{-# LANGUAGE DeriveAnyClass #-}
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
import Vira.Lib.Git (BranchName, CommitID)
import Vira.State.Type
import Vira.State.Type qualified as T

{- | Application that gets persisted to disk through acid-state

All operations (`query` or `update`) on this state are defined immediately below. They can be invoked as follows:

>>> Just repo <- Vira.App.query $ GetRepoByNameA "my-repo"

Data in this state is indexed by `IxSet` to allow for efficient querying.
-}
data ViraState = ViraState
  { repos :: IxRepo
  , branches :: IxBranch
  , jobs :: IxJob
  }
  deriving stock (Generic, Typeable)

$(deriveSafeCopy 0 'base ''ViraState)

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
      { repos = Ix.updateIx (name repo) repo s.repos
      }

-- | Set a repository's branches
setRepoBranchesA :: RepoName -> Map BranchName CommitID -> Update ViraState ()
setRepoBranchesA repo branches = do
  modify $ \s ->
    s
      { branches = Ix.insertList (Map.toList branches <&> uncurry (Branch repo)) s.branches
      }

-- | Get all jobs of a repo's branch
getJobsByBranchA :: RepoName -> BranchName -> Query ViraState [Job]
getJobsByBranchA repo branch = do
  ViraState {jobs} <- ask
  pure $ Ix.toList $ jobs @= repo @= branch

-- | Create a new job returning it.
addNewJobA :: RepoName -> BranchName -> CommitID -> Update ViraState Job
addNewJobA jobRepo jobBranch jobCommit = do
  jobs <- Ix.toList <$> gets jobs
  let
    jobId =
      let ids = T.jobId <$> jobs
       in if Prelude.null ids then JobId 1 else JobId 1 + maximum ids
    jobStatus = JobPending
    jobLog = ""
    job = Job {..}
  modify $ \s ->
    s
      { jobs = Ix.insert job s.jobs
      }
  pure job

jobUpdateStatusA :: JobId -> JobStatus -> Text -> Update ViraState ()
jobUpdateStatusA jobId status log = do
  modify $ \s -> do
    let job = fromMaybe (error $ "No such job: " <> show jobId) $ Ix.getOne $ s.jobs @= jobId
    s
      { jobs = Ix.updateIx jobId (job {jobStatus = status, jobLog = log}) s.jobs
      }

markRunningJobsAsStaleA :: Update ViraState ()
markRunningJobsAsStaleA = do
  jobs <- Ix.toList <$> gets jobs
  forM_ jobs $ \job -> do
    case job.jobStatus of
      JobRunning -> do
        jobUpdateStatusA job.jobId JobKilled ""
      _ -> pass

$( makeAcidic
    ''ViraState
    [ 'getAllReposA
    , 'getRepoByNameA
    , 'getBranchesByRepoA
    , 'getBranchByNameA
    , 'setRepoA
    , 'setRepoBranchesA
    , 'getJobsByBranchA
    , 'addNewJobA
    , 'jobUpdateStatusA
    , 'markRunningJobsAsStaleA
    ]
 )
