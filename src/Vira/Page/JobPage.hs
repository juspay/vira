{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (asks)
import Htmx.Servant.Response
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.Logging
import Vira.Lib.Git (BranchName)
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (RepoName)
import Vira.Supervisor qualified as Supervisor
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- List all jobs for a repo
    _list :: mode :- Get '[HTML] (Html ())
  , -- Trigger a new build
    _build :: mode :- "new" :> Capture "branch" BranchName :> Post '[HTML] (Headers '[HXRefresh] Text)
  }
  deriving stock (Generic)

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg repoName = do
  Routes
    { _list = App.runAppInServant cfg $ listHandler repoName
    , _build = App.runAppInServant cfg . buildHandler repoName
    }

listHandler :: RepoName -> Eff App.AppServantStack (Html ())
listHandler repoName = do
  branches <- App.query $ St.GetBranchesByRepoA repoName
  xs <- forM branches $ \branch -> do
    jobs <- App.query $ St.GetJobsByBranchA repoName branch.branchName
    pure (branch, jobs)
  pure $ forM_ xs $ \(branch, jobs) -> do
    h2_ $ toHtml $ "Jobs for " <> show @Text branch.branchName
    ul_ $ forM_ jobs $ \job -> do
      li_ $ toHtml $ show @Text job

buildHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch >>= \case
    Nothing -> throwError err404
    Just () -> pure $ addHeader True "Ok"

-- TODO:
-- 1. Fail if a build is already happening (until we support queuing)
-- 2. Contact supervisor to spawn a new build, with it status going to DB.
triggerNewBuild :: (HasCallStack) => RepoName -> BranchName -> Eff App.AppServantStack (Maybe ())
triggerNewBuild repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError err404) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError err404) pure
  log Info $ "Building commit " <> show (repoName, branch.headCommit)
  asks App.supervisor >>= \supervisor -> do
    -- TODO We need a concept of 'working copy' to which source should be checked out. Then `nix build .` on that.
    taskId <- Supervisor.startTask supervisor $ "nix build --no-link --print-out-paths " <> toString (gitFlakeUrl repo.cloneUrl) <> "/" <> toString branch.headCommit
    -- TODO: Update db with new job
    log Info $ "Started task " <> show taskId
  pure $ Just ()
  where
    gitFlakeUrl :: Text -> Text
    gitFlakeUrl _url =
      -- TODO: Implement this more generally
      "github:srid/haskell-template"
