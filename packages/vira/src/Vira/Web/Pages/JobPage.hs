{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Web.Pages.JobPage where

import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Git (BranchName, RepoName)
import Effectful.Reader.Dynamic (asks)
import GHC.IO.Exception (ExitCode (..))
import Htmx.Servant.Response
import Lucid
import Servant hiding (throwError)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.CI.Environment (environmentFor, workspacePath)
import Vira.CI.Pipeline qualified as Pipeline
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.Logging
import Vira.Lib.TimeExtra (formatDuration)
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (JobId, jobWorkingDir)
import Vira.Supervisor.Task qualified as Supervisor
import Vira.Supervisor.Type (Terminated (Terminated))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl, runAppHtml)
import Vira.Web.Pages.JobLog qualified as JobLog
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Button qualified as W
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Status qualified as W
import Vira.Web.Widgets.Time qualified as Time
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

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings = do
  Routes
    { _build = \x -> Web.runAppInServant globalSettings viraRuntimeState webSettings . buildHandler x
    , _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml . viewHandler
    , _log = JobLog.handlers globalSettings viraRuntimeState webSettings
    , _kill = Web.runAppInServant globalSettings viraRuntimeState webSettings . killHandler
    }

buildHandler :: RepoName -> BranchName -> Eff Web.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch
  pure $ addHeader True "Ok"

viewHandler :: JobId -> AppHtml ()
viewHandler jobId = do
  job <- lift $ App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  let crumbs =
        [ LinkTo.RepoListing
        , LinkTo.Repo job.repo
        , LinkTo.RepoBranch job.repo job.branch
        , LinkTo.Job jobId
        ]
  W.layout crumbs $ viewJob job

killHandler :: JobId -> Eff Web.AppServantStack (Headers '[HXRefresh] Text)
killHandler jobId = do
  supervisor <- asks App.supervisor
  Supervisor.killTask supervisor jobId
  pure $ addHeader True "Killed"

viewJob :: St.Job -> AppHtml ()
viewJob job = do
  let jobActive = St.jobIsActive job

  W.viraSection_ [] $ do
    W.viraPageHeader_ ("Job #" <> (toText @String $ show job.jobId)) $ do
      div_ [class_ "space-y-4"] $ do
        -- Top row: commit info and actions
        div_ [class_ "flex items-center justify-between"] $ do
          div_ [class_ "flex items-center space-x-4"] $ do
            span_ "Commit:"
            W.viraCommitInfo_ job.commit
          div_ [class_ "flex items-center space-x-4"] $ do
            viewJobStatus job.jobStatus
            when jobActive $ do
              killLink <- lift $ getLink $ LinkTo.Kill job.jobId
              W.viraRequestButton_
                W.ButtonDestructive
                killLink
                [title_ "Kill this job"]
                $ do
                  W.viraButtonIcon_ $ toHtmlRaw Icon.ban
                  "Kill Job"

        -- Bottom row: timing information
        div_ [class_ "flex items-center space-x-6 text-sm text-gray-600 dark:text-gray-300 border-t dark:border-gray-700 pt-3"] $ do
          div_ [class_ "flex items-center space-x-2"] $ do
            span_ [class_ "font-medium"] "Started:"
            Time.viraUTCTime_ job.jobCreatedTime
          case St.jobEndTime job of
            Just endTime -> do
              div_ [class_ "flex items-center space-x-2"] $ do
                span_ [class_ "font-medium"] "Finished:"
                Time.viraUTCTime_ endTime
              div_ [class_ "flex items-center space-x-2"] $ do
                span_ [class_ "font-medium"] "Duration:"
                let duration = diffUTCTime endTime job.jobCreatedTime
                span_
                  [ class_ "cursor-help"
                  , title_ "Total time from job creation to completion"
                  ]
                  $ toHtml
                  $ formatDuration duration
            Nothing ->
              div_ [class_ "flex items-center space-x-2"] $ do
                span_ [class_ "font-medium"] "Status:"
                case job.jobStatus of
                  St.JobStale -> span_ [class_ "text-gray-500 dark:text-gray-400"] "Stale"
                  St.JobPending -> span_ [class_ "text-gray-500 dark:text-gray-400"] "In progress"
                  St.JobRunning -> span_ [class_ "text-gray-500 dark:text-gray-400"] "In progress"
                  St.JobFinished {} -> span_ [class_ "text-gray-500 dark:text-gray-400"] "In progress" -- Should not happen due to outer case

    -- Job logs
    W.viraCard_ [class_ "p-6"] $ do
      JobLog.view job

viewJobHeader :: St.Job -> AppHtml ()
viewJobHeader job = do
  jobUrl <- lift $ getLinkUrl $ LinkTo.Job job.jobId
  a_ [title_ "View Job Details", href_ jobUrl, class_ "block"] $ do
    div_ [class_ "flex items-center justify-between"] $ do
      div_ [class_ "flex items-center space-x-4"] $ do
        div_ [class_ "flex-shrink-0"] $ do
          span_ [class_ "inline-flex items-center px-3 py-1 rounded-full text-sm font-semibold bg-gray-100 dark:bg-gray-700 text-gray-800 dark:text-gray-200"] $ do
            "Job #" <> toHtml (show @Text job.jobId)
        W.viraCommitInfo_ job.commit
      viewJobStatus job.jobStatus

viewJobStatus :: (Monad m) => St.JobStatus -> HtmlT m ()
viewJobStatus status = do
  W.viraStatusBadge_ status

-- TODO:
-- 1. Fail if a build is already happening (until we support queuing)
-- 2. Contact supervisor to spawn a new build, with it status going to DB.
triggerNewBuild :: (HasCallStack) => RepoName -> BranchName -> Eff Web.AppServantStack ()
triggerNewBuild repoName branchName = do
  repo <- App.query (St.GetRepoByNameA repoName) >>= maybe (throwError $ err404 {errBody = "No such repo"}) pure
  branch <- App.query (St.GetBranchByNameA repoName branchName) >>= maybe (throwError $ err404 {errBody = "No such branch"}) pure
  log Info $ "Building commit " <> show (repoName, branch.headCommit)
  asks App.supervisor >>= \supervisor -> do
    creationTime <- liftIO getCurrentTime
    let baseDir = Workspace.repoJobsDir supervisor repo.name
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit baseDir creationTime
    log Info $ "Added job " <> show job
    viraEnv <- environmentFor repo branch job.jobWorkingDir
    Supervisor.startTask supervisor job.jobId viraEnv.workspacePath (Pipeline.runPipeline viraEnv) $ \result -> do
      endTime <- liftIO getCurrentTime
      let status = case result of
            Right ExitSuccess -> St.JobFinished St.JobSuccess endTime
            Right (ExitFailure _code) -> St.JobFinished St.JobFailure endTime
            Left (Pipeline.PipelineTerminated Terminated) -> St.JobFinished St.JobKilled endTime
            Left _ -> St.JobFinished St.JobFailure endTime
      App.update $ St.JobUpdateStatusA job.jobId status
    App.update $ St.JobUpdateStatusA job.jobId St.JobRunning
    log Info $ "Started task " <> show job.jobId
