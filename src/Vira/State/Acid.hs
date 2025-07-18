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
import System.FilePath ((</>))
import Vira.Lib.Git (BranchName, CommitID)
import Vira.State.Type
import Vira.State.Type qualified as T

{- | Application that gets persisted to disk through acid-state

All operations (`query` or `update`) on this state are defined immediately below. They can be invoked as follows:

>>> Just repo <- Vira.App.query $ GetRepoByNameA "my-repo"

Data in this state is indexed by `IxSet` to allow for efficient querying.
-}
data ViraState = ViraState
  { branches :: IxBranch
  , jobs :: IxJob
  , appSettings :: AppSettings
  }
  deriving stock (Generic, Typeable)

$(deriveSafeCopy 0 'base ''ViraState)

setAppSettingsA :: AppSettings -> Update ViraState ()
setAppSettingsA appSettings = do
  modify $ \s ->
    s {appSettings = appSettings}

getAppSettingsA :: Query ViraState AppSettings
getAppSettingsA = do
  ViraState {appSettings} <- ask
  pure appSettings

getCachixSettingsA :: Query ViraState (Maybe CachixSettings)
getCachixSettingsA = do
  ViraState {appSettings} <- ask
  pure appSettings.cachix

getAtticSettingsA :: Query ViraState (Maybe AtticSettings)
getAtticSettingsA = do
  ViraState {appSettings} <- ask
  pure appSettings.attic

setCachixSettingsA :: CachixSettings -> Update ViraState ()
setCachixSettingsA cachix = do
  modify $ \s ->
    s
      { -- TODO: wouldn't it be nice to use { appSettings.repos = IX.fromList repos} instead
        appSettings = s.appSettings {cachix = Just cachix}
      }

setAtticSettingsA :: AtticSettings -> Update ViraState ()
setAtticSettingsA attic = do
  modify $ \s ->
    s
      { -- TODO: wouldn't it be nice to use { appSettings.repos = IX.fromList repos} instead
        appSettings = s.appSettings {attic = Just attic}
      }

setAllReposA :: [Repo] -> Update ViraState ()
setAllReposA repos = do
  modify $ \s ->
    s
      { -- TODO: wouldn't it be nice to use { appSettings.repos = IX.fromList repos} instead
        appSettings = s.appSettings {repos = Ix.fromList repos}
      }

addNewRepoA :: Repo -> Update ViraState ()
addNewRepoA repo = do
  modify $ \s ->
    s
      { appSettings = s.appSettings {repos = Ix.insert repo s.appSettings.repos}
      }

deleteRepoA :: Repo -> Update ViraState ()
deleteRepoA repo = do
  modify $ \s ->
    s
      { appSettings = s.appSettings {repos = Ix.insert repo s.appSettings.repos}
      }

-- | Get all repositories
getAllReposA :: Query ViraState [Repo]
getAllReposA = do
  ViraState {appSettings} <- ask
  pure $ Ix.toList appSettings.repos

-- | Get a repository by name
getRepoByNameA :: RepoName -> Query ViraState (Maybe Repo)
getRepoByNameA name = do
  ViraState {appSettings} <- ask
  pure $ Ix.getOne $ appSettings.repos @= name

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
      { -- TODO: Same as setAllReposA todo
        appSettings = s.appSettings {repos = Ix.updateIx (name repo) repo s.appSettings.repos}
      }

-- | Set a repository's branches
setRepoBranchesA :: RepoName -> Map BranchName CommitID -> Update ViraState ()
setRepoBranchesA repo branches = do
  modify $ \s ->
    let
      repoBranches = Map.toList branches <&> uncurry (Branch repo)
     in
      s
        { branches = updateIxMulti repo (Ix.fromList repoBranches) s.branches
        }

-- | Get all jobs of a repo's branch in descending order
getJobsByBranchA :: RepoName -> BranchName -> Query ViraState [Job]
getJobsByBranchA repo branch = do
  ViraState {jobs} <- ask
  pure $ Ix.toDescList (Proxy @JobId) $ jobs @= repo @= branch

-- | Get all running jobs
getRunningJobs :: Query ViraState [Job]
getRunningJobs = do
  ViraState {jobs} <- ask
  pure $ Ix.toList $ jobs @+ [JobPending, JobRunning]

getJobA :: JobId -> Query ViraState (Maybe Job)
getJobA jobId = do
  ViraState {jobs} <- ask
  pure $ Ix.getOne $ jobs @= jobId

-- | Create a new job returning it.
addNewJobA :: RepoName -> BranchName -> CommitID -> FilePath -> Update ViraState Job
addNewJobA jobRepo jobBranch jobCommit baseWorkDir = do
  jobs <- Ix.toList <$> gets jobs
  let
    jobId =
      let ids = T.jobId <$> jobs
       in if Prelude.null ids then JobId 1 else JobId 1 + maximum ids
    jobStatus = JobPending
    jobWorkingDir = baseWorkDir </> toString jobRepo </> show jobId
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
    case job.jobStatus of
      JobPending -> do
        jobUpdateStatusA job.jobId JobKilled
      JobRunning -> do
        jobUpdateStatusA job.jobId JobKilled
      _ -> pass

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
    , 'getJobsByBranchA
    , 'getRunningJobs
    , 'getJobA
    , 'addNewJobA
    , 'jobUpdateStatusA
    , 'markUnfinishedJobsAsStaleA
    , 'setAppSettingsA
    , 'getAppSettingsA
    , 'getCachixSettingsA
    , 'setCachixSettingsA
    , 'getAtticSettingsA
    , 'setAtticSettingsA
    , 'addNewRepoA
    , 'deleteRepoA
    ]
 )
