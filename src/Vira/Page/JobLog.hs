{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.JobLog where

import Control.Concurrent (threadDelay)
import Effectful (Eff)
import Effectful.Error.Static (throwError)
import Htmx.Lucid.Core (hxSwap_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream (ServerEvent (ServerEvent), ServerSentEvents, ToServerEvent (toServerEvent))
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.HTMX (hxSseClose_, hxSseConnect_, hxSseSwap_)
import Vira.Lib.Process.TailF (TailF)
import Vira.Lib.Process.TailF qualified as TailF
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Vira.State.Type qualified as St
import Vira.Status qualified as Status
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

data LogChunk
  = Chunk Int (Html ())
  | Stop Int (Html ())

logChunkType :: LogChunk -> Text
logChunkType = \case
  Chunk _ _ -> "logchunk"
  Stop _ _ -> "logstop"

logChunkId :: LogChunk -> LByteString
logChunkId = \case
  Chunk ident _ -> show ident
  Stop ident _ -> show ident

logChunkMsg :: LogChunk -> LByteString
logChunkMsg = \case
  Chunk _ html -> Lucid.renderBS html
  Stop _ html -> Lucid.renderBS html

instance ToServerEvent LogChunk where
  toServerEvent chunk =
    ServerEvent
      (Just $ encodeUtf8 $ logChunkType chunk)
      (Just $ logChunkId chunk)
      (logChunkMsg chunk)

data StreamState = Init | Streaming TailF | Stopping

streamHandler :: App.AppState -> JobId -> S.SourceT IO LogChunk
streamHandler cfg jobId = S.fromStepT $ step 0 Init
  where
    step (n :: Int) (st :: StreamState) = S.Effect $ do
      job :: Job <-
        App.runApp cfg $
          App.query (St.GetJobA jobId)
            >>= maybe (error "Job not found") pure
      let logFile = job.jobWorkingDir </> "output.log"
      case st of
        Init -> do
          logTail <- liftIO $ TailF.new logFile
          streamLog n job logTail
        Streaming logTail -> do
          streamLog n job logTail
        Stopping -> do
          -- Keep going unless client has time to catch up.
          threadDelay 1000_000
          pure $ S.Yield (stopStep n) $ step (n + 1) st
    streamLog n job logTail = do
      let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
      mLine <- TailF.tryRead logTail
      case (mLine, jobActive) of
        (Nothing, True) -> do
          -- Job is active, but log yet to be updated; retry.
          threadDelay 100_000
          pure $ S.Skip $ step n (Streaming logTail)
        (Nothing, False) -> do
          -- Job ended
          -- TODO: Drain all of tail -f's output first
          -- Otherwise, we will miss last few lines of logs!
          putStrLn "Job ended; ending stream"
          TailF.stop logTail
          pure $ S.Yield (stopStep n) $ step (n + 1) Stopping
        (Just line, _) -> do
          let msg = Chunk n (pre_ $ toHtml line)
          pure $ S.Yield msg $ step (n + 1) (Streaming logTail)
    stopStep n = Stop n $ do
      Status.indicator False

viewStream :: (LinkTo.LinkTo -> Link) -> St.Job -> Html ()
viewStream linkTo job = do
  div_ $ do
    div_ [class_ "my-2"] $ do
      p_ $ do
        a_
          [target_ "blank", class_ "underline text-blue-500", href_ $ show . linkURI $ linkTo $ LinkTo.JobLog job.jobId]
          "View Full Log"
    let streamLink = show . linkURI $ linkTo $ LinkTo.JobLogStream job.jobId
    let sseAttrs =
          [ hxExt_ "sse"
          , hxSseConnect_ streamLink
          , hxSwap_ "beforeend show:window:bottom"
          , hxSseClose_ $ logChunkType $ Stop 0 mempty
          ]
    div_ sseAttrs $ do
      pre_
        [ hxSseSwap_ $ logChunkType $ Chunk 0 mempty
        , class_ "bg-black text-white p-2 text-xs"
        , style_ "white-space: pre-wrap;"
        ]
        $ code_
        $ do
          "Loading log ..."
      div_
        [ hxSseSwap_ $ logChunkType $ Stop 0 mempty
        , hxSwap_ "innerHTML"
        ]
        $ do
          Status.indicator True
