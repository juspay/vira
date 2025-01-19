{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (ask, asks)
import GHC.IO.Exception (ExitCode (..))
import Htmx.Servant.Response
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.LinkTo qualified as LinkTo
import Vira.App.Logging
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (JobId, RepoName, jobWorkingDir)
import Vira.Supervisor qualified as Supervisor
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Trigger a new build
    _build :: mode :- "new" :> Capture "repo" RepoName :> Capture "branch" BranchName :> Post '[HTML] (Headers '[HXRefresh] Text)
  , -- View a job
    _view :: mode :- Capture "job" JobId :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _build = \x -> App.runAppInServant cfg . buildHandler x
    , _view = App.runAppInServant cfg . viewHandler
    }

buildHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch
  pure $ addHeader True "Ok"

viewHandler :: JobId -> Eff App.AppServantStack (Html ())
viewHandler jobId = do
  cfg <- ask
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <- case jobWorkingDir job.jobStatus of
    Nothing -> pure ""
    Just workDir -> do
      -- TODO: Streaming!
      liftIO $ readFileBS $ workDir </> "output.log"
  pure $ W.layout cfg.linkTo (show jobId) [LinkTo.Job jobId] $ do
    viewJob cfg.linkTo job
    div_ $ do
      pre_ [class_ "bg-black text-white p-2 text-xs"] $ code_ $ do
        toHtml logText

viewJob :: (LinkTo.LinkTo -> Link) -> St.Job -> Html ()
viewJob linkTo job = do
  a_ [title_ "View Job Details", href_ $ show . linkURI $ linkTo $ LinkTo.Job job.jobId] $ do
    div_ [class_ "flex items-center justify-start space-x-4 hover:bg-blue-100"] $ do
      div_ [class_ "w-24"] $ do
        b_ $ "Job #" <> toHtml (show @Text job.jobId)
      viewCommit job.jobCommit
      viewJobStatus job.jobStatus

viewCommit :: Git.CommitID -> Html ()
viewCommit (Git.CommitID commit) = do
  code_ [class_ "text-gray-700 hover:text-black"] $ toHtml commit

viewJobStatus :: St.JobStatus -> Html ()
viewJobStatus status = do
  case status of
    St.JobRunning _ -> span_ [class_ "text-blue-700"] "🚧 Running"
    St.JobPending -> span_ [class_ "text-yellow-700"] "⏳ Pending"
    St.JobFinished _ St.JobSuccess -> span_ [class_ "text-green-700"] "✅ Success"
    St.JobFinished _ St.JobFailure -> span_ [class_ "text-red-700"] "❌ Failure"
    St.JobKilled _ -> span_ [class_ "text-red-700"] "💀 Killed"

-- TODO:
-- 1. Fail if a build is already happening (until we support queuing)
-- 2. Contact supervisor to spawn a new build, with it status going to DB.
triggerNewBuild :: (HasCallStack) => RepoName -> BranchName -> Eff App.AppServantStack ()
triggerNewBuild repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError $ err404 {errBody = "No such repo"}) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError $ err404 {errBody = "No such branch"}) pure
  log Info $ "Building commit " <> show (repoName, branch.headCommit)
  asks App.supervisor >>= \supervisor -> do
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit
    log Info $ "Added job " <> show job
    let cmd = "nix build -L --no-link --print-out-paths " <> toString (gitFlakeUrl repo.cloneUrl) <> "/" <> toString branch.headCommit
    workDir <- Supervisor.startTask supervisor job.jobId cmd $ \workDir exitCode -> do
      let status = case exitCode of
            ExitSuccess -> St.JobFinished workDir St.JobSuccess
            ExitFailure _code -> St.JobFinished workDir St.JobFailure
      App.update $ St.JobUpdateStatusA job.jobId status
    App.update $ St.JobUpdateStatusA job.jobId $ St.JobRunning workDir
    log Info $ "Started task " <> show job.jobId
  where
    gitFlakeUrl :: Text -> Text
    gitFlakeUrl _url =
      -- TODO: Implement this more generally
      "github:srid/emanote"
