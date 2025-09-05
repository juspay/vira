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
  { repos :: IxRepo
  , branches :: IxBranch
  , jobs :: IxJob
  , cachix :: Maybe CachixSettings
  -- ^ Global Cachix settings, i.e for all the `repos`
  , attic :: Maybe AtticSettings
  -- ^ Global Attic settings, i.e for all the `repos`
  , remoteBuilders :: IxRemoteBuilder
  -- ^ Remote builders for distributed builds
  }
  deriving stock (Generic, Typeable)

$(deriveSafeCopy 0 'base ''ViraState)

-- | Get the cachix settings
getCachixSettingsA :: Query ViraState (Maybe CachixSettings)
getCachixSettingsA = do
  ViraState {cachix} <- ask
  pure cachix

-- | Get the attic settings
getAtticSettingsA :: Query ViraState (Maybe AtticSettings)
getAtticSettingsA = do
  ViraState {attic} <- ask
  pure attic

-- | Set the cachix settings
setCachixSettingsA :: Maybe CachixSettings -> Update ViraState ()
setCachixSettingsA mCachix = do
  modify $ \s ->
    s
      { cachix = mCachix
      }

-- | Set the attic settings
setAtticSettingsA :: Maybe AtticSettings -> Update ViraState ()
setAtticSettingsA mAttic = do
  modify $ \s ->
    s
      { attic = mAttic
      }

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
      { repos = Ix.updateIx (name repo) repo s.repos
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

-- | Get all jobs of a repository (across all branches) in descending order by JobId
getJobsByRepoA :: RepoName -> Query ViraState [Job]
getJobsByRepoA repo = do
  ViraState {jobs} <- ask
  pure $ Ix.toDescList (Proxy @JobId) $ jobs @= repo

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

-- | Get all remote builders
getAllRemoteBuildersA :: Query ViraState [RemoteBuilder]
getAllRemoteBuildersA = do
  ViraState {remoteBuilders} <- ask
  pure $ Ix.toList remoteBuilders

-- | Get a remote builder by ID
getRemoteBuilderByIdA :: RemoteBuilderId -> Query ViraState (Maybe RemoteBuilder)
getRemoteBuilderByIdA builderId = do
  ViraState {remoteBuilders} <- ask
  pure $ Ix.getOne $ remoteBuilders @= builderId

-- | Add a new remote builder
addRemoteBuilderA :: Text -> Text -> [Platform] -> Maybe Text -> Update ViraState RemoteBuilder
addRemoteBuilderA user host platforms note = do
  builders <- Ix.toList <$> gets remoteBuilders
  let
    builderId =
      let ids = remoteBuilderId <$> builders
       in if Prelude.null ids then RemoteBuilderId 1 else RemoteBuilderId 1 + maximum ids
    builder = RemoteBuilder user host platforms note builderId
  modify $ \s ->
    s
      { remoteBuilders = Ix.insert builder s.remoteBuilders
      }
  pure builder

-- | Update a remote builder
updateRemoteBuilderA :: RemoteBuilderId -> Text -> Text -> [Platform] -> Maybe Text -> Update ViraState ()
updateRemoteBuilderA builderId user host platforms note = do
  modify $ \s -> do
    let builder = fromMaybe (error $ "No such remote builder: " <> show builderId) $ Ix.getOne $ s.remoteBuilders @= builderId
        updatedBuilder = builder {remoteBuilderUser = user, remoteBuilderHost = host, remoteBuilderPlatforms = platforms, remoteBuilderNote = note}
    s
      { remoteBuilders = Ix.updateIx builderId updatedBuilder s.remoteBuilders
      }

-- | Delete a remote builder by ID
deleteRemoteBuilderA :: RemoteBuilderId -> Update ViraState ()
deleteRemoteBuilderA builderId = do
  modify $ \s ->
    s
      { remoteBuilders = Ix.deleteIx builderId s.remoteBuilders
      }

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
    , 'getJobsByRepoA
    , 'getRunningJobs
    , 'getJobA
    , 'addNewJobA
    , 'jobUpdateStatusA
    , 'markUnfinishedJobsAsStaleA
    , 'getCachixSettingsA
    , 'setCachixSettingsA
    , 'getAtticSettingsA
    , 'setAtticSettingsA
    , 'addNewRepoA
    , 'deleteRepoByNameA
    , 'getAllRemoteBuildersA
    , 'getRemoteBuilderByIdA
    , 'addRemoteBuilderA
    , 'updateRemoteBuilderA
    , 'deleteRemoteBuilderA
    ]
 )
