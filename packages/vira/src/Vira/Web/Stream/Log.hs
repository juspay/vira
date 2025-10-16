{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time log streeaming
module Vira.Web.Stream.Log (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * View
  viewStream,
  logViewerWidget,
  renderLogLines,
) where

import Colog (Severity (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.CircularBuffer (CircularBuffer)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.Map qualified as Map
import Effectful (Eff)
import Effectful.Reader.Dynamic (asks)
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseClose_, hxSseConnect_, hxSseSwap_, hyperscript_)
import Servant.API.EventStream
import Servant.API.Stream (SourceIO)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import System.Tail qualified as Tail
import Vira.App qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.CI.Log (decodeViraLog)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId)
import Vira.State.Type qualified as St
import Vira.Supervisor.Type (Task (..), TaskInfo (tailHandle), TaskSupervisor (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Widgets.Status qualified as Status

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
  Chunk _ logLines -> Lucid.renderBS $ rawMultiLine (toList logLines)
  Stop _ -> Lucid.renderBS $
    div_ [class_ "flex items-center space-x-2 text-sm text-gray-600"] $ do
      Status.indicator False
      span_ [class_ "font-medium"] "Build completed"

-- | Render log lines with special styling for viralog lines (JSON format)
renderLogLines :: (Monad m) => [Text] -> HtmlT m ()
renderLogLines ls =
  -- Use `lines . unlines` to normalize: each Text may contain embedded newlines,
  -- so we flatten them into individual lines to check each for viralog prefix
  let allLines = lines $ unlines ls
   in mconcat $ map renderLine allLines
  where
    renderLine :: (Monad m) => Text -> HtmlT m ()
    renderLine line =
      case decodeViraLog line of
        Right viraLog -> toHtml viraLog
        Left _ -> toHtml line <> br_ []

-- | Render multiline lines for placing under a <pre> such that newlines are preserved & rendered
rawMultiLine :: (Monad m) => [Text] -> HtmlT m ()
rawMultiLine = renderLogLines

instance ToServerEvent LogChunk where
  toServerEvent chunk =
    ServerEvent
      (Just $ encodeUtf8 $ logChunkType chunk)
      (Just $ logChunkId chunk)
      (logChunkMsg chunk)

data StreamState = Init | Streaming (CircularBuffer Text) | StreamEnding | Stopping

streamRouteHandler :: JobId -> SourceT (Eff AppStack) LogChunk
streamRouteHandler jobId = S.fromStepT $ S.Effect $ do
  tagStreamThread
  pure $ S.Skip $ step 0 Init
  where
    step (n :: Int) (st :: StreamState) = S.Effect $ do
      mJob :: Maybe Job <- App.query (St.GetJobA jobId)
      case mJob of
        Nothing -> do
          App.log Error "Job not found"
          pure $ S.Error "Job not found"
        Just job -> do
          handleState job n st
    handleState job n st =
      case st of
        Init -> do
          -- Get the task from supervisor to reuse its Tail handle
          supervisor <- Effectful.Reader.Dynamic.asks supervisor
          tasks <- liftIO $ readMVar supervisor.tasks
          case Map.lookup jobId tasks of
            Nothing -> do
              App.log Error $ "Task not found in supervisor for job " <> show jobId
              pure $ S.Error "Task not found in supervisor"
            Just task -> do
              -- Subscribe to the existing Tail handle
              queue <- liftIO $ Tail.tailSubscribe task.info.tailHandle
              streamLog n job queue
        Streaming queue -> do
          streamLog n job queue
        StreamEnding -> do
          pure $ S.Yield (Stop n) $ step (n + 1) Stopping
        Stopping -> do
          -- Keep going until the htmx client has time to catch up.
          liftIO $ threadDelay 1_000_000
          pure $ S.Yield (Stop n) $ step (n + 1) st
    streamLog n job queue = do
      liftIO (atomically (CB.drain queue)) >>= \case
        Nothing -> do
          -- Queue is closed, no more lines will come. End the stream.
          App.log Info $ "Job " <> show job.jobId <> " log queue closed; ending stream"
          pure $ S.Yield (Stop n) $ step (n + 1) Stopping
        Just availableLines -> do
          -- Send all available lines as a single chunk
          pure $ S.Yield (Chunk n availableLines) $ step (n + 1) (Streaming queue)

viewStream :: St.Job -> AppHtml ()
viewStream job = do
  streamLink <- lift $ getLinkUrl $ LinkTo.JobLogStream job.jobId
  div_
    [ hxExt_ "sse"
    , hxSseConnect_ streamLink
    , hxSseClose_ $ logChunkType $ Stop 0
    ]
    $ do
      -- Hidden div to handle log chunk SSE events
      div_
        [ hxSseSwap_ $ logChunkType $ Chunk 0 (one "")
        , hxSwap_ "beforeend"
        , hxTarget_ ("#" <> sseTarget)
        , style_ "display: none;"
        ]
        pass

      -- Hidden div to handle stop SSE events
      div_
        [ hxSseSwap_ $ logChunkType $ Stop 0
        , hxSwap_ "innerHTML"
        , hxTarget_ "#streaming-status"
        , style_ "display: none;"
        ]
        pass

      logViewerWidget job $ do
        "Loading log ..."
        br_ mempty

      -- Streaming status area
      div_
        [ id_ "streaming-status"
        , class_ "flex items-center justify-center py-3 border-t border-gray-200 bg-gray-50 rounded-b-lg"
        ]
        $ do
          div_ [class_ "flex items-center space-x-2 text-sm text-gray-600"] $ do
            Status.indicator True
            span_ [class_ "font-medium"] "Streaming build logs..."

-- | Log viewer widget supporting both static and streaming context.
logViewerWidget :: Job -> (forall m. (Monad m) => HtmlT m ()) -> AppHtml ()
logViewerWidget job w = do
  jobLogUrl <- lift $ getLinkUrl $ LinkTo.JobLog job.jobId
  div_ [class_ "space-y-4"] $ do
    -- Header with actions
    div_ [class_ "flex items-center justify-between"] $ do
      h4_ [class_ "text-sm font-semibold text-gray-700 uppercase tracking-wider"] "Build Output"
      a_
        [ target_ "blank"
        , class_ "text-sm text-gray-600 hover:text-indigo-600 transition-colors font-medium"
        , href_ jobLogUrl
        ]
        "View Full Log →"

    -- Log container with improved styling
    div_
      [ id_ "logContainer"
      , class_ "h-[60vh] overflow-y-auto overflow-x-hidden bg-gray-900 rounded-t-lg border border-b-0 border-gray-200 shadow-sm"
      , -- Auto-scroll to bottom when content changes (SSE updates, page load, or any DOM mutations)
        hyperscript_ "on htmx:afterSwap or load or mutation set my scrollTop to my scrollHeight"
      ]
      $ do
        pre_
          [ id_ sseTarget
          , class_ "text-gray-100 p-4 text-sm h-full whitespace-pre-wrap break-all m-0 font-mono leading-relaxed"
          ]
          $ do
            code_ w

-- | ID of the HTML element targeted by SSE message swaps (log streaming)
sseTarget :: Text
sseTarget = "logViewerWidget-pre"
