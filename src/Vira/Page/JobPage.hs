{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Process (CreateProcess (cwd), env, proc)
import Effectful.Reader.Dynamic (ask, asks)
import GHC.IO.Exception (ExitCode (..))
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (AfterEnd))
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging
import Vira.Lib.Attic
import Vira.Lib.Cachix
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.Lib.Omnix qualified as Omnix
import Vira.Page.JobLog qualified as JobLog
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (JobId, RepoName, jobWorkingDir)
import Vira.Supervisor qualified as Supervisor
import Vira.Supervisor.Type (TaskException (UserKilled), TaskSupervisor (baseWorkDir))
import Vira.Widgets qualified as W
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
    , _view = App.runAppInServant cfg . viewHandler
    , _log = JobLog.handlers cfg
    , _kill = App.runAppInServant cfg . killHandler
    }

buildHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch
  pure $ addHeader True "Ok"

viewHandler :: JobId -> Eff App.AppServantStack (Html ())
viewHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  let crumbs =
        [ LinkTo.RepoListing
        , LinkTo.Repo job.jobRepo
        , LinkTo.RepoBranch job.jobRepo job.jobBranch
        , LinkTo.Job jobId
        ]
  cfg <- ask
  W.layout cfg crumbs <$> viewJob cfg.linkTo job

killHandler :: JobId -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
killHandler jobId = do
  supervisor <- asks App.supervisor
  Supervisor.killTask supervisor jobId
  pure $ addHeader True "Killed"

viewJob :: (LinkTo.LinkTo -> Link) -> St.Job -> Eff App.AppServantStack (Html ())
viewJob linkTo job = do
  let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
  logView <- JobLog.view linkTo job
  pure $ do
    viewJobHeader linkTo job
    logView
    when jobActive $
      W.viraButton_
        [ hxPostSafe_ $ linkTo $ LinkTo.Kill job.jobId
        , hxSwapS_ AfterEnd
        ]
        "Kill"

viewJobHeader :: (LinkTo.LinkTo -> Link) -> St.Job -> Html ()
viewJobHeader linkTo job = do
  a_ [title_ "View Job Details", href_ $ show . linkURI $ linkTo $ LinkTo.Job job.jobId] $ do
    div_ [class_ "flex items-center justify-start space-x-4 hover:bg-blue-100"] $ do
      div_ [class_ "w-24"] $ do
        b_ $ "Job #" <> toHtml (show @Text job.jobId)
      viewCommit job.jobCommit
      viewJobStatus job.jobStatus

viewCommit :: Git.CommitID -> Html ()
viewCommit (Git.CommitID commit) = do
  code_ [class_ "text-gray-700 text-sm hover:text-black"] $ toHtml commit

viewJobStatus :: St.JobStatus -> Html ()
viewJobStatus status = do
  case status of
    St.JobRunning -> span_ [class_ "text-blue-700"] "🚧 Running"
    St.JobPending -> span_ [class_ "text-yellow-700"] "⏳ Pending"
    St.JobFinished St.JobSuccess -> span_ [class_ "text-green-700"] "✅ Success"
    St.JobFinished St.JobFailure -> span_ [class_ "text-red-700"] "❌ Failure"
    St.JobKilled -> span_ [class_ "text-red-700"] "💀 Killed"

-- TODO:
-- 1. Fail if a build is already happening (until we support queuing)
-- 2. Contact supervisor to spawn a new build, with it status going to DB.
triggerNewBuild :: (HasCallStack) => RepoName -> BranchName -> Eff App.AppServantStack ()
triggerNewBuild repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError $ err404 {errBody = "No such repo"}) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError $ err404 {errBody = "No such branch"}) pure
  log Info $ "Building commit " <> show (repoName, branch.headCommit)
  appSettings <- asks App.settings
  let mCachix = App.cachix . App.repo $ appSettings
  let mAttic = App.attic . App.repo $ appSettings
  asks App.supervisor >>= \supervisor -> do
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit supervisor.baseWorkDir
    log Info $ "Added job " <> show job
    let stages = getStages repo branch mCachix mAttic
    Supervisor.startTask supervisor job.jobId job.jobWorkingDir stages $ \result -> do
      let status = case result of
            Right ExitSuccess -> St.JobFinished St.JobSuccess
            Right (ExitFailure _code) -> St.JobFinished St.JobFailure
            Left UserKilled -> St.JobKilled
      App.update $ St.JobUpdateStatusA job.jobId status
    App.update $ St.JobUpdateStatusA job.jobId St.JobRunning
    log Info $ "Started task " <> show job.jobId

-- | Get all build stages
getStages :: St.Repo -> St.Branch -> Maybe App.CachixSettings -> Maybe App.AtticSettings -> NonEmpty CreateProcess
getStages repo branch mCachix mAttic = do
  stageCreateProjectDir
    :| stagesClone
    <> maybe [] (one . stageAtticLogin) mAttic
    <> [stageBuild]
    <> (maybe mempty (one . stageCachixPush) mCachix <> maybe mempty (one . stageAtticPush) mAttic)
  where
    stageCreateProjectDir =
      proc "mkdir" ["project"]
    stagesClone =
      Git.cloneAtCommit repo.cloneUrl branch.branchName branch.headCommit
        <&> \p -> p {cwd = Just "project"}
    stageBuild =
      Omnix.omnixCiProcess
        { cwd = Just "project"
        }
    -- Run the stage before any other attic processes
    stageAtticLogin attic =
      (atticLoginProcess attic.atticServer attic.atticToken)
        { cwd = Just "project"
        }
    stageCachixPush cachix =
      (cachixPushProcess cachix.cachixName "result")
        { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
        , cwd = Just "project"
        }
    stageAtticPush attic =
      (atticPushProcess attic.atticServer attic.atticCacheName "result")
        { cwd = Just "project"
        }
