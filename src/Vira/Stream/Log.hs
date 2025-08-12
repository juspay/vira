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
import Data.Map.Strict qualified as Map
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant hiding (throwError)
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Logging (Severity (Error, Info))
import Vira.Lib.HTMX (hxSseClose_, hxSseConnect_, hxSseSwap_)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId, jobWorkingDir)
import Vira.State.Type qualified as St
import Vira.Stream.Status qualified as Status
import Vira.Supervisor.LogBroadcast qualified as LogBroadcast
import Vira.Supervisor.Type (tasks)

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

-- | Stream state machine for log streaming
data StreamState
  = -- | Initial state: find task and set up broadcaster
    Init
  | -- | Actively streaming logs to client
    Streaming (TBQueue Text)
  | -- | Job finished: keep sending Stop messages to ensure client receives final logs
    Stopping

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
          -- Get the task from supervisor and create or get the shared log broadcaster
          taskMap <- liftIO $ readMVar (tasks (App.supervisor cfg))
          case Map.lookup jobId taskMap of
            Nothing -> do
              App.runApp cfg $ App.log Error $ "Task not found in supervisor: " <> show jobId
              pure $ S.Error "Task not found in supervisor"
            Just task -> do
              broadcaster <- LogBroadcast.getOrCreateLogBroadcaster task logFile
              clientQueue <- LogBroadcast.subscribeToLogs broadcaster
              streamLog n job clientQueue
        Streaming clientQueue -> do
          streamLog n job clientQueue
        Stopping -> do
          -- Keep sending Stop messages with delay to ensure HTMX client has time to
          -- process any final log chunks before the SSE connection closes
          threadDelay 1_000_000
          pure $ S.Yield (Stop n) $ step (n + 1) Stopping
    streamLog n job clientQueue = do
      let jobActive = job.jobStatus == St.JobRunning || job.jobStatus == St.JobPending
      LogBroadcast.tryReadLogQueue clientQueue >>= \case
        Just line -> do
          let msg = Chunk n line
          pure $ S.Yield msg $ step (n + 1) (Streaming clientQueue)
        Nothing -> case jobActive of
          True -> do
            -- Job is active, but no log available now; retry.
            threadDelay 100_000
            pure $ S.Skip $ step n (Streaming clientQueue)
          False -> do
            -- Job ended; transition to Stopping state to ensure final logs are processed
            App.runApp cfg $ do
              App.log Info $ "Job " <> show job.jobId <> " ended; ending stream"
            pure $ S.Yield (Stop n) $ step (n + 1) Stopping

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
