{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobLog where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream (recommendedEventSourceHeaders)
import Servant.Server.Generic (AsServer)
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.Lucid (VHtml)
import Vira.App.Servant (runVSourceIO)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Vira.State.Type qualified as St
import Vira.Stream.Log qualified as Log
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Raw build log
    _rawLog :: mode :- "raw" :> Get '[PlainText] Text
  , -- Stream build log
    _streamLog :: mode :- "stream" :> Log.StreamRoute
  }
  deriving stock (Generic)

handlers :: App.AppState -> JobId -> Routes AsServer
handlers cfg jobId = do
  Routes
    { _rawLog = App.runAppInServant cfg $ rawLogHandler jobId
    , _streamLog = pure $ recommendedEventSourceHeaders $ runVSourceIO cfg $ Log.streamRouteHandler jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText

view :: Job -> VHtml ()
view job = do
  let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
  if jobActive
    then Log.viewStream job
    else viewStaticLog job

viewStaticLog :: Job -> VHtml ()
viewStaticLog job = do
  logText <- readJobLogFull job
  Log.logViewerWidget job $ do
    toHtml logText

readJobLogFull :: (MonadIO m) => Job -> m Text
readJobLogFull job = do
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText
