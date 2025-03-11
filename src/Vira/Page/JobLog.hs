{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobLog where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Servant hiding (throwError)
import Servant.Server.Generic (AsServer)
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type (JobId, jobWorkingDir)
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
    , _streamLog = pure $ Log.streamRouteHandler cfg jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText
