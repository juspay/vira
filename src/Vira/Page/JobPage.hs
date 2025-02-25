{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobPage where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Effectful.Process (CreateProcess (cwd), env, proc)
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
import Vira.Lib.Cachix
import Vira.Lib.Git (BranchName)
import Vira.Lib.Git qualified as Git
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (JobId, RepoName, jobWorkingDir)
import Vira.Supervisor qualified as Supervisor
import Vira.Supervisor.Type (TaskSupervisor (baseWorkDir))
import Vira.Widgets qualified as W
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Trigger a new build
    _build :: mode :- "new" :> Capture "repo" RepoName :> Capture "branch" BranchName :> Post '[HTML] (Headers '[HXRefresh] Text)
  , -- View a job
    _view :: mode :- Capture "job" JobId :> Get '[HTML] (Html ())
  , -- Raw build log
    _rawLog :: mode :- Capture "job" JobId :> "log" :> Get '[PlainText] Text
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg = do
  Routes
    { _build = \x -> App.runAppInServant cfg . buildHandler x
    , _view = App.runAppInServant cfg . viewHandler
    , _rawLog = App.runAppInServant cfg . rawLogHandler
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText

buildHandler :: RepoName -> BranchName -> Eff App.AppServantStack (Headers '[HXRefresh] Text)
buildHandler repoName branch = do
  triggerNewBuild repoName branch
  pure $ addHeader True "Ok"

viewHandler :: JobId -> Eff App.AppServantStack (Html ())
viewHandler jobId = do
  cfg <- ask
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    -- TODO: Streaming!
    liftIO $ fmap (decodeUtf8 @Text) . readFileBS $ job.jobWorkingDir </> "output.log"
  let crumbs = [LinkTo.RepoListing, LinkTo.Repo job.jobRepo, LinkTo.Job jobId]
  pure $ W.layout cfg (show jobId) crumbs $ do
    viewJob cfg.linkTo job
    div_ $ do
      let linesToShow = 20
      div_ [class_ "my-2"] $ do
        p_ $ do
          "Displaying only last " <> show linesToShow <> " lines of the log. "
          a_
            [target_ "blank", class_ "underline text-blue-500", href_ $ show . linkURI $ cfg.linkTo $ LinkTo.JobLog job.jobId]
            "View Full Log"
        p_ "NOTE: You must refresh this page, since logs are not being streamed (yet)"
      pre_ [class_ "bg-black text-white p-2 text-xs"] $ code_ $ do
        "[..]\n"
        toHtml $ getLastNLines linesToShow logText

getLastNLines :: Int -> Text -> Text
getLastNLines n = unlines . lastN n . lines
  where
    lastN :: Int -> [a] -> [a]
    lastN n' xs = drop (length xs - n') xs

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
  mCachix <- asks $ App.cachix . App.repo . App.settings
  asks App.supervisor >>= \supervisor -> do
    job <- App.update $ St.AddNewJobA repoName branchName branch.headCommit supervisor.baseWorkDir
    log Info $ "Added job " <> show job
    let stages =
          stageCreateProjectDir
            :| stagesClone repo branch
            <> [stageBuild]
            <> maybe mempty (one . stageCachix) mCachix
    Supervisor.startTask supervisor job.jobId job.jobWorkingDir stages $ \exitCode -> do
      let status = case exitCode of
            ExitSuccess -> St.JobFinished St.JobSuccess
            ExitFailure _code -> St.JobFinished St.JobFailure
      App.update $ St.JobUpdateStatusA job.jobId status
    App.update $ St.JobUpdateStatusA job.jobId St.JobRunning
    log Info $ "Started task " <> show job.jobId
  where
    stageCreateProjectDir =
      proc "mkdir" ["project"]
    stagesClone repo branch =
      Git.cloneAtCommit repo.cloneUrl branch.branchName branch.headCommit
        <&> \p -> p {cwd = Just "project"}
    stageBuild =
      Omnix.omnixCiProcess
        { cwd = Just "project"
        }
    stageCachix cachix =
      (cachixPushProcess cachix.cachixName "result")
        { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
        , cwd = Just "project"
        }
