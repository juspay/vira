{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Vira.Page.JobLog where

import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream (ServerEvent (ServerEvent), ServerSentEvents, ToServerEvent (toServerEvent))
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import System.IO (hClose, hGetLine, openFile)
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Prelude hiding (ask, asks)

data Routes mode = Routes
  { -- Raw build log
    _rawLog :: mode :- "raw" :> Get '[PlainText] Text
  , -- Stream build log
    _streamLog :: mode :- "stream" :> ServerSentEvents (SourceIO LogChunk)
  }
  deriving stock (Generic)

handlers :: App.AppState -> JobId -> Routes AsServer
handlers cfg jobId = do
  Routes
    { _rawLog = App.runAppInServant cfg $ rawLogHandler jobId
    , _streamLog = pure $ streamHandler cfg jobId
    }

rawLogHandler :: JobId -> Eff App.AppServantStack Text
rawLogHandler jobId = do
  job <- App.query (St.GetJobA jobId) >>= maybe (throwError err404) pure
  logText <-
    liftIO $ readFileBS $ job.jobWorkingDir </> "output.log"
  pure $ decodeUtf8 logText

data LogChunk = LogChunk Int (Maybe Handle) (Html ())

instance ToServerEvent LogChunk where
  toServerEvent (LogChunk ident _handle t) =
    ServerEvent
      (Just "logchunk")
      (Just $ show ident)
      (Lucid.renderBS t)

streamHandler :: App.AppState -> JobId -> S.SourceT IO LogChunk
streamHandler cfg jobId = S.fromStepT $ step 0 Nothing
  where
    step (n :: Int) (mh :: Maybe Handle) = S.Effect $ do
      job :: Job <- App.runApp cfg $ App.query (St.GetJobA jobId) >>= maybe undefined pure
      let logFile = job.jobWorkingDir </> "output.log"
      h <- maybe (liftIO $ openFile logFile ReadMode) pure mh
      -- TODO:
      -- 1. Read last N lines, rather than reading from scratch
      -- 2. Send chunks, not single line. And delay?
      liftIO (hIsEOF h) >>= \case
        True -> do
          liftIO $ hClose h
          pure S.Stop
        False -> do
          chunk <- liftIO $ hGetLine h
          let msg = LogChunk n (Just h) (pre_ $ toHtml chunk)
          pure $ S.Yield msg $ step (n + 1) (Just h)
