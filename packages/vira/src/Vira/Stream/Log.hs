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

import Colog (Severity (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.CircularBuffer (CircularBuffer)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.Map qualified as Map
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseClose_, hxSseConnect_, hxSseSwap_, hyperscript_)
import Servant hiding (throwError)
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import System.Tail qualified as Tail
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId)
import Vira.State.Type qualified as St
import Vira.Stream.Status qualified as Status
import Vira.Supervisor.Type (Task (..), TaskSupervisor (..))

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO LogChunk))

-- | SSE message for log streaming
data LogChunk
  = -- A chunk of log data
    Chunk Int (NonEmpty Text)
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
  Chunk _ logLines -> Lucid.renderBS $ pre_ $ toHtml $ unlines $ toList logLines
  Stop _ -> Lucid.renderBS $
    div_ [class_ "flex items-center space-x-2 text-sm text-gray-600"] $ do
      Status.indicator False
      span_ [class_ "font-medium"] "Build completed"

instance ToServerEvent LogChunk where
  toServerEvent chunk =
    ServerEvent
      (Just $ encodeUtf8 $ logChunkType chunk)
      (Just $ logChunkId chunk)
      (logChunkMsg chunk)

data StreamState = Init | Streaming (CircularBuffer Text) | StreamEnding | Stopping

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
          -- Get the task from supervisor to reuse its Tail handle
          tasks <- liftIO $ readMVar cfg.supervisor.tasks
          case Map.lookup jobId tasks of
            Nothing -> do
              App.runApp cfg $ App.log Error $ "Task not found in supervisor for job " <> show jobId
              pure $ S.Error "Task not found in supervisor"
            Just task -> do
              -- Subscribe to the existing Tail handle
              queue <- liftIO $ Tail.tailSubscribe task.tailHandle
              streamLog n job queue
        Streaming queue -> do
          streamLog n job queue
        StreamEnding -> do
          pure $ S.Yield (Stop n) $ step (n + 1) Stopping
        Stopping -> do
          -- Keep going until the htmx client has time to catch up.
          threadDelay 1_000_000
          pure $ S.Yield (Stop n) $ step (n + 1) st
    streamLog n job queue = do
      atomically (CB.drain queue) >>= \case
        Nothing -> do
          -- Queue is closed, no more lines will come. End the stream.
          App.runApp cfg $ do
            App.log Info $ "Job " <> show job.jobId <> " log queue closed; ending stream"
          pure $ S.Yield (Stop n) $ step (n + 1) Stopping
        Just availableLines -> do
          -- Send all available lines as a single chunk
          pure $ S.Yield (Chunk n availableLines) $ step (n + 1) (Streaming queue)

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
      [ hxSseSwap_ $ logChunkType $ Chunk 0 (one "")
      , hxSwap_ "beforeend"
      , hxTarget_ ("#" <> sseTarget)
      ]
      $ do
        logViewerWidget linkTo job $ do
          "Loading log ..."
    -- Div containing streaming status
    div_
      [ hxSseSwap_ $ logChunkType $ Stop 0
      , hxSwap_ "innerHTML"
      , class_ "flex items-center justify-center py-3 border-t border-gray-200 bg-gray-50 rounded-b-lg"
      ]
      $ do
        div_ [class_ "flex items-center space-x-2 text-sm text-gray-600"] $ do
          Status.indicator True
          span_ [class_ "font-medium"] "Streaming build logs..."

-- | Log viewer widget agnostic to static or streaming nature.
logViewerWidget :: (LinkTo.LinkTo -> Link) -> Job -> Html () -> Html ()
logViewerWidget linkTo job w = do
  div_ [class_ "space-y-4"] $ do
    -- Header with actions
    div_ [class_ "flex items-center justify-between"] $ do
      h4_ [class_ "text-sm font-semibold text-gray-700 uppercase tracking-wider"] "Build Output"
      a_
        [ target_ "blank"
        , class_ "text-sm text-gray-600 hover:text-indigo-600 transition-colors font-medium"
        , href_ $ show . linkURI $ linkTo $ LinkTo.JobLog job.jobId
        ]
        "View Full Log →"

    -- Log container with improved styling
    div_
      [ id_ "logContainer"
      , class_ "h-[60vh] overflow-y-auto overflow-x-hidden bg-gray-900 rounded-t-lg border border-b-0 border-gray-200 shadow-sm"
      ]
      $ do
        pre_
          [ id_ sseTarget
          , class_ "text-gray-100 p-4 text-sm h-full whitespace-pre-wrap break-all m-0 font-mono leading-relaxed"
          , hyperscript_ "on htmx:afterSwap set #logContainer.scrollTop to #logContainer.scrollHeight"
          ]
          $ do
            code_ w

-- | ID of the HTML element targeted by SSE message swaps (log streaming)
sseTarget :: Text
sseTarget = "logViewerWidget-pre"
