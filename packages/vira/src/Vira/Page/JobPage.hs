{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Colog (Severity (..))
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Dynamic (asks)
import GHC.IO.Exception (ExitCode (..))
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_)
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.Logging
import Vira.Page.JobLog qualified as JobLog
import Vira.Repo.Core (getStages)
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (JobId, RepoName, jobWorkingDir)
import Vira.Supervisor.Task qualified as Supervisor
import Vira.Supervisor.Type (TaskException (KilledByUser), TaskSupervisor (baseWorkDir))
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Code qualified as W
import Vira.Widgets.Layout qualified as W
import Vira.Widgets.Status qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Trigger a new build
    _build :: mode :- "new" :> Capture "repo" RepoName :> Capture "branch" BranchName :> Post '[HTML] (Headers '[HXRefresh] Text)
  , -- View a job
    _view :: mode :- Capture "job" JobId :> Get '[HTML] (Html ())
  , -- Log routes
    _log :: mode :- Capture "job" JobId :> "log" :> NamedRoutes JobLog.Routes
  , -- Kill an active job
    _kill :: mode :- Capture "job" JobId :> Post '[HTML] (Headers '[HXRefresh] Text)
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _build = \x -> App.runAppInServant cfg . buildHandler x
    , _view = App.runAppInServant cfg . App.runAppHtml . viewHandler
    , _log = JobLog.handlers cfg
    , _kill = App.runAppInServant cfg . killHandler
    }

buildHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch
  pure $ addHeader True "Ok"

viewHandler :: JobId -> AppHtml ()
viewHandler jobId = do
  job <- lift $ App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  let crumbs =
        [ LinkTo.RepoListing
        , LinkTo.Repo job.jobRepo
        , LinkTo.RepoBranch job.jobRepo job.jobBranch
        , LinkTo.Job jobId
        ]
  W.layout crumbs $ viewJob job

killHandler :: JobId -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
killHandler jobId = do
  supervisor <- asks App.supervisor
  Supervisor.killTask supervisor jobId
  pure $ addHeader True "Killed"

viewJob :: St.Job -> App.AppHtml ()
viewJob job = do
  let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending

  W.viraSection_ [] $ do
    W.viraPageHeader_ ("Job #" <> (toText @String $ show job.jobId)) $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "flex items-center space-x-4"] $ do
          span_ "Commit:"
          viewCommit job.jobCommit
        div_ [class_ "flex items-center space-x-4"] $ do
          viewJobStatus job.jobStatus
          when jobActive $ do
            killLink <- lift $ App.getLink $ LinkTo.Kill job.jobId
            W.viraButton_
              W.ButtonDestructive
              [ hxPostSafe_ killLink
              , hxSwapS_ AfterEnd
              ]
              $ do
                W.viraButtonIcon_ $ toHtmlRaw Icon.ban
                "Kill Job"

    -- Job logs
    W.viraCard_ [class_ "p-6"] $ do
      JobLog.view job

viewJobHeader :: St.Job -> App.AppHtml ()
viewJobHeader job = do
  jobUrl <- lift $ App.getLinkUrl $ LinkTo.Job job.jobId
  a_ [title_ "View Job Details", href_ jobUrl, class_ "block"] $ do
    div_ [class_ "flex items-center justify-between"] $ do
      div_ [class_ "flex items-center space-x-4"] $ do
        div_ [class_ "flex-shrink-0"] $ do
          span_ [class_ "inline-flex items-center px-3 py-1 rounded-full text-sm font-semibold bg-gray-100 text-gray-800"] $ do
            "Job #" <> toHtml (show @Text job.jobId)
        viewCommit job.jobCommit
      viewJobStatus job.jobStatus

viewCommit :: (Monad m) => Git.CommitID -> HtmlT m ()
viewCommit (Git.CommitID commit) = do
  W.viraCodeInline_ (T.take 8 $ toText commit)

viewJobStatus :: (Monad m) => St.JobStatus -> HtmlT m ()
viewJobStatus status = do
  W.viraStatusBadge_ status

-- TODO:
-- 1. Fail if a build is already happening (until we support queuing)
-- 2. Contact supervisor to spawn a new build, with it status going to DB.
triggerNewBuild :: (HasCallStack) => RepoName -> BranchName -> Eff App.AppServantStack ()
triggerNewBuild repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError $ err404 {errBody = "No such repo"}) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError $ err404 {errBody = "No such branch"}) pure
  log Info $ "Building commit " <> show (repoName, branch.headCommit)
  mCachix <- App.query St.GetCachixSettingsA
  mAttic <- App.query St.GetAtticSettingsA
  asks App.supervisor >>= \supervisor -> do
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit supervisor.baseWorkDir
    log Info $ "Added job " <> show job
    let
      stages = getStages repo branch mCachix mAttic
    Supervisor.startTask supervisor job.jobId job.jobWorkingDir stages $ \result -> do
      let status = case result of
            Right ExitSuccess -> St.JobFinished St.JobSuccess
            Right (ExitFailure _code) -> St.JobFinished St.JobFailure
            Left KilledByUser -> St.JobKilled
      App.update $ St.JobUpdateStatusA job.jobId status
    App.update $ St.JobUpdateStatusA job.jobId St.JobRunning
    log Info $ "Started task " <> show job.jobId
