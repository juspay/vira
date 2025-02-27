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
import Prelude hiding (ask, asks)

newtype Routes mode = Routes
  { -- Raw build log
    _rawLog :: mode :- "log" :> Get '[PlainText] Text
  }
  deriving stock (Generic)

handlers :: App.AppState -> JobId -> Routes AsServer
handlers cfg jobId = do
  Routes
    { _rawLog = App.runAppInServant cfg $ rawLogHandler jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText
