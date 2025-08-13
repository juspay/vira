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
import Control.Concurrent.STM (TBQueue)
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging (Severity (Error, Info))
import Vira.Lib.FileTailer qualified as FileTailer
import Vira.Lib.HTMX (hxSseClose_, hxSseConnect_, hxSseSwap_)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId)
import Vira.State.Type qualified as St
import Vira.Stream.Status qualified as Status
import Vira.Supervisor.Task qualified as Supervisor

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

streamRouteHandler :: App.AppState -> JobId -> SourceIO LogChunk
streamRouteHandler cfg jobId = S.fromStepT $ step 0 Nothing
  where
    stopStreaming n = S.Yield (Stop n) S.Stop

    step (n :: Int) (mQueue :: Maybe (TBQueue (NonEmpty Text))) = S.Effect $ do
      mJob :: Maybe Job <- App.runApp cfg $ App.query (St.GetJobA jobId)
      case mJob of
        Nothing -> do
          App.runApp cfg $ App.log Error "Job not found"
          pure $ stopStreaming n
        Just job -> do
          streamLog n job mQueue

    streamLog n job = \case
      Just clientQueue ->
        -- Already initialized, continue streaming
        streamWithQueue n job clientQueue
      Nothing -> do
        -- Initialize: set up the tailer and get client queue
        mClientQueue <- App.runApp cfg $ Supervisor.tailTaskLog (App.supervisor cfg) jobId
        case mClientQueue of
          Nothing -> do
            App.runApp cfg $ App.log Error $ "Task not found in supervisor: " <> show jobId
            pure $ stopStreaming n
          Just clientQueue ->
            streamWithQueue n job clientQueue

    streamWithQueue n job clientQueue = do
      let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
      FileTailer.tryReadTailQueue clientQueue >>= \case
        Just linesBatch -> do
          -- Send the whole batch as a single chunk
          let batchText = unlines (toList linesBatch)
              msg = Chunk n batchText
          pure $ S.Yield msg $ step (n + 1) (Just clientQueue)
        Nothing -> case jobActive of
          True -> do
            -- Job is active, but no log available now; retry.
            threadDelay 100_000
            pure $ S.Skip $ step n (Just clientQueue)
          False -> do
            -- Job ended; send Stop message and end stream
            App.runApp cfg $ do
              App.log Info $ "Job " <> show job.jobId <> " ended; ending stream"
            pure $ stopStreaming n

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
