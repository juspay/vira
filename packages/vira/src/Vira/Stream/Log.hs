{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time log streeaming
module Vira.Stream.Log (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * View
  viewStream,
  logViewerWidget,
) where

import Control.Concurrent (threadDelay)
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseClose_, hxSseConnect_, hxSseSwap_)
import Servant hiding (throwError)
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import System.TailF (TailF)
import System.TailF qualified as TailF
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging (Severity (Error, Info))
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Vira.State.Type qualified as St
import Vira.Stream.Status qualified as Status

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO LogChunk))

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

streamRouteHandler :: App.AppState -> JobId -> SourceIO LogChunk
streamRouteHandler cfg jobId = S.fromStepT $ step 0 Init
  where
    step (n :: Int) (st :: StreamState) = S.Effect $ do
      mJob :: Maybe Job <- App.runApp cfg $ App.query (St.GetJobA jobId)
      case mJob of
        Nothing -> do
          App.runApp cfg $ App.log Error "Job not found"
          pure $ S.Error "Job not found"
        Just job -> do
          handleState job n st
    handleState job n st =
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
          -- Keep going until the htmx client has time to catch up.
          threadDelay 1_000_000
          pure $ S.Yield (Stop n) $ step (n + 1) st
    streamLog n job logTail = do
      let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
      TailF.tryRead logTail >>= \case
        Just line -> do
          let msg = Chunk n line
          pure $ S.Yield msg $ step (n + 1) (Streaming logTail)
        Nothing -> case jobActive of
          True -> do
            -- Job is active, but no log available now; retry.
            threadDelay 100_000
            pure $ S.Skip $ step n (Streaming logTail)
          False -> do
            -- Job ended; let's wrap up.
            App.runApp cfg $ do
              App.log Info $ "Job " <> show job.jobId <> " ended; ending stream"
            TailF.stop logTail >>= \case
              Nothing ->
                pure $ S.Yield (Stop n) $ step (n + 1) Stopping
              Just s ->
                -- Flush last set of log lines.
                pure $ S.Yield (Chunk n s) $ step (n + 1) StreamEnding

viewStream :: (LinkTo.LinkTo -> Link) -> St.Job -> Html ()
viewStream linkTo job = do
  let streamLink = show . linkURI $ linkTo $ LinkTo.JobLogStream job.jobId
      sseAttrs =
        [ hxExt_ "sse"
        , hxSseConnect_ streamLink
        , hxSseClose_ $ logChunkType $ Stop 0
        ]
  div_ sseAttrs $ do
    -- Div containing log messages
    div_
      [ hxSseSwap_ $ logChunkType $ Chunk 0 mempty
      , hxSwap_ "beforeend show:window:bottom"
      , hxTarget_ ("#" <> sseTarget)
      ]
      $ do
        logViewerWidget linkTo job $ do
          "Loading log ..."
    -- Div containing streaming status
    div_
      [ hxSseSwap_ $ logChunkType $ Stop 0
      , hxSwap_ "innerHTML"
      ]
      $ do
        Status.indicator True

-- | Log viewer widget agnostic to static or streaming nature.
logViewerWidget :: (LinkTo.LinkTo -> Link) -> Job -> Html () -> Html ()
logViewerWidget linkTo job w = do
  div_ $ do
    div_ [class_ "my-2"] $ do
      p_ $ do
        a_
          [target_ "blank", class_ "underline text-blue-500", href_ $ show . linkURI $ linkTo $ LinkTo.JobLog job.jobId]
          "View Full Log"
    pre_
      [ id_ sseTarget
      , class_ "bg-black text-white p-2 text-xs"
      , style_ "white-space: pre-wrap;"
      ]
      $ do
        code_ w

-- | ID of the HTML element targeted by SSE message swaps (log streaming)
sseTarget :: Text
sseTarget = "logViewerWidget-pre"
