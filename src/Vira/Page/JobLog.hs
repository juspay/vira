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
import Vira.App.Logging (Severity (Error, Info))
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

-- | SSE message for log streaming
data LogChunk
  = -- A chunk of log data
    Chunk Int Text
  | -- Last message, indicating streaming has ended
    Stop Int

logChunkType :: LogChunk -> Text
logChunkType = \case
  Chunk _ _ -> "logchunk"
  Stop _ -> "logstop"

logChunkId :: LogChunk -> LByteString
logChunkId = \case
  Chunk ident _ -> show ident
  Stop ident -> show ident

logChunkMsg :: LogChunk -> LByteString
logChunkMsg = \case
  Chunk _ line -> Lucid.renderBS $ pre_ $ toHtml line
  Stop _ -> Lucid.renderBS $ Status.indicator False

instance ToServerEvent LogChunk where
  toServerEvent chunk =
    ServerEvent
      (Just $ encodeUtf8 $ logChunkType chunk)
      (Just $ logChunkId chunk)
      (logChunkMsg chunk)

data StreamState = Init | Streaming TailF | StreamEnding | Stopping

streamHandler :: App.AppState -> JobId -> S.SourceT IO LogChunk
streamHandler cfg jobId = S.fromStepT $ step 0 Init
  where
    step (n :: Int) (st :: StreamState) = S.Effect $ do
      mJob :: Maybe Job <- App.runApp cfg $ App.query (St.GetJobA jobId)
      case mJob of
        Nothing -> do
          App.runApp cfg $ App.log Error "Job not found"
          pure $ S.Error "Job not found"
        Just job -> do
          case st of
            Init -> do
              let logFile = job.jobWorkingDir </> "output.log"
              logTail <- liftIO $ TailF.new logFile
              streamLog n job logTail
            Streaming logTail -> do
              streamLog n job logTail
            StreamEnding -> do
              pure $ S.Yield (Stop n) $ step (n + 1) Stopping
            Stopping -> do
              -- Keep going unless client has time to catch up.
              threadDelay 1_000_000
              pure $ S.Yield (Stop n) $ step (n + 1) st
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
          App.runApp cfg $ do
            App.log Info $ "Job " <> show job.jobId <> " ended; ending stream"
          TailF.stop logTail >>= \case
            Nothing ->
              pure $ S.Yield (Stop n) $ step (n + 1) Stopping
            Just s ->
              pure $ S.Yield (Chunk n s) $ step (n + 1) StreamEnding
        (Just line, _) -> do
          let msg = Chunk n line
          pure $ S.Yield msg $ step (n + 1) (Streaming logTail)

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
          , hxSseClose_ $ logChunkType $ Stop 0
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
        [ hxSseSwap_ $ logChunkType $ Stop 0
        , hxSwap_ "innerHTML"
        ]
        $ do
          Status.indicator True
