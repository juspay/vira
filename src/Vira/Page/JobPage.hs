{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (asks)
import GHC.IO.Exception (ExitCode (..))
import Htmx.Servant.Response
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
import Vira.Supervisor.Type (TaskOutput (..))
import Prelude hiding (ask, asks)

newtype Routes mode = Routes
  { -- Trigger a new build
    _build :: mode :- "new" :> Capture "branch" BranchName :> Post '[HTML] (Headers '[HXRefresh] Text)
  }
  deriving stock (Generic)

handlers :: App.AppState -> RepoName -> Routes AsServer
handlers cfg repoName = do
  Routes
    { _build = App.runAppInServant cfg . buildHandler repoName
    }

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
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit
    log Info $ "Added job " <> show job
    -- TODO We need a concept of 'working copy' to which source should be checked out. Then `nix build .` on that.
    let cmd = "nix build -L --no-link --print-out-paths " <> toString (gitFlakeUrl repo.cloneUrl) <> "/" <> toString branch.headCommit
    taskId <- Supervisor.startTask supervisor job.jobId cmd $ \taskOutput -> do
      -- TODO: Set stdout
      let status = case taskOutput.exitCode of
            ExitSuccess -> St.JobFinished St.JobSuccess
            ExitFailure _code -> St.JobFinished St.JobFailure
      App.update $ St.JobUpdateStatusA job.jobId status $ toText taskOutput.output
    App.update $ St.JobUpdateStatusA job.jobId St.JobRunning ""
    log Info $ "Started task " <> show taskId
  pure $ Just ()
  where
    gitFlakeUrl :: Text -> Text
    gitFlakeUrl _url =
      -- TODO: Implement this more generally
      "github:srid/haskell-template"
