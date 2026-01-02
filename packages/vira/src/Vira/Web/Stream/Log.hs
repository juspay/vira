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
import Data.Map qualified as Map
import Effectful (Eff)
import Effectful.Reader.Dynamic (asks)
import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxExt_)
import LogSink.Broadcast (CircularBuffer, bcSubscribe, drain)
import Lucid
import Lucid.Htmx.Contrib (hxSseClose_, hxSseConnect_, hxSseSwap_, hyperscript_)
import Servant.API.EventStream
import Servant.API.Stream (SourceIO)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import System.Nix.Logging.Noise qualified as NixNoise
import Vira.App qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.CI.Log (decodeViraLog)
import Vira.State.Acid qualified as St
import Vira.State.Type (Job, JobId)
import Vira.State.Type qualified as St
import Vira.Supervisor.Type (Task (..), TaskInfo (broadcast), TaskSupervisor (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Vira.Web.Widgets.Status qualified as Status

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO (KeepAlive LogChunk)))

-- | SSE message for log streaming
data LogChunk
  = -- | A chunk of log data
    Chunk Int (NonEmpty Text)
  | -- | Last message, indicating streaming has ended
    Stop Int

-- | Get event type for 'LogChunk'
logChunkType :: LogChunk -> Text
logChunkType = \case
  Chunk _ _ -> "logchunk"
  Stop _ -> "logstop"

-- | Get event ID for 'LogChunk'
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
      span_ [class_ "font-medium"] "Log streaming stopped"

-- | Render log lines with special styling for viralog lines (JSON format)
renderLogLines :: (Monad m) => [Text] -> HtmlT m ()
renderLogLines ls =
  -- Each chunk already has a trailing newline from the sink, so use mconcat instead of unlines
  -- to avoid adding extra newlines that would create empty lines between entries
  let allLines = lines $ mconcat ls
      lineGroups = NixNoise.groupNixNoiseLines allLines
   in mconcat $ map renderLineGroup lineGroups
  where
    -- Render a line group (either collapsible block or regular line)
    renderLineGroup :: (Monad m) => NixNoise.LineGroup -> HtmlT m ()
    renderLineGroup (NixNoise.RegularLine line) = renderLine line
    renderLineGroup (NixNoise.NixNoiseBlock []) = mempty -- Skip empty blocks
    renderLineGroup (NixNoise.NixNoiseBlock nixLines) =
      details_ [class_ "opacity-60 hover:opacity-100 transition-opacity"] $ do
        summary_ [class_ "cursor-pointer text-slate-500 dark:text-slate-600 select-none"] $
          toHtml ("○ Nix input details (" <> show (length nixLines) <> " lines)" :: Text)
        div_ [class_ "pl-4 text-sm opacity-70"] $
          mconcat $
            map renderLine nixLines

    -- Render individual line
    renderLine :: (Monad m) => Text -> HtmlT m ()
    renderLine line = do
      case decodeViraLog line of
        Right viraLog -> do
          toHtml viraLog
          br_ []
        Left _ -> do
          toHtml line
          br_ []

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

data StreamConfig = StreamConfig
  { counter :: Int
  , job :: Job
  , streamState :: StreamState
  }

data JobLookup = Active Job | Stale

-- | Lookup a job and check if it's active or stale.
lookupActiveJob :: JobId -> Eff AppStack (Maybe JobLookup)
lookupActiveJob jobId = do
  mJob <- App.query (St.GetJobA jobId)
  pure $ case mJob of
    Nothing -> Nothing
    Just job ->
      if St.jobIsActive job
        then Just (Active job)
        else Just Stale

streamRouteHandler :: JobId -> SourceT (Eff AppStack) (KeepAlive LogChunk)
streamRouteHandler jobId = S.fromStepT $ S.Effect $ do
  tagStreamThread
  lookupActiveJob jobId >>= \case
    Nothing -> do
      App.log Error "Job not found"
      pure $ S.Error "Job not found"
    Just Stale -> do
      -- Job is stale/finished - send Stop event to close SSE connection gracefully
      -- This triggers hx-sse-close and prevents reconnection attempts
      App.log Warning $ "SSE stream requested for inactive job " <> show jobId <> ", closing stream"
      pure $ KeepAlive.yieldEvent (Stop 0) S.Stop
    Just (Active job) -> do
      pure $ S.Skip $ step StreamConfig {counter = 0, job, streamState = Init}
  where
    step cfg = S.Effect $ do
      case cfg.streamState of
        Init -> do
          -- Get the task from supervisor to reuse its Tail handle
          supervisor <- Effectful.Reader.Dynamic.asks supervisor
          tasks <- liftIO $ readMVar supervisor.tasks
          case Map.lookup jobId tasks of
            Nothing -> do
              App.log Error $ "Task not found in supervisor for job " <> show jobId
              pure $ S.Error "Task not found in supervisor"
            Just task -> do
              -- Subscribe to the existing Broadcast handle for SSE streaming
              queue <- liftIO $ bcSubscribe task.info.broadcast
              streamLog cfg queue
        Streaming queue -> do
          streamLog cfg queue
        StreamEnding -> do
          pure $ KeepAlive.yieldEvent (Stop cfg.counter) $ step (cfg {counter = cfg.counter + 1, streamState = Stopping})
        Stopping -> do
          -- Keep going until the htmx client has time to catch up.
          liftIO $ threadDelay 1_000_000
          pure $ KeepAlive.yieldEvent (Stop cfg.counter) $ step cfg
    streamLog cfg queue =
      pure $
        KeepAlive.withKeepAlive
          (atomically $ drain queue)
          ( \case
              Nothing -> do
                -- Queue is closed, no more lines will come. End the stream.
                App.log Info $ "Job " <> show cfg.job.jobId <> " log queue closed; ending stream"
                pure $ Just (Stop cfg.counter, cfg {counter = cfg.counter + 1, streamState = Stopping})
              Just availableLines -> do
                -- Send all available lines as a single chunk
                pure $ Just (Chunk cfg.counter availableLines, cfg {counter = cfg.counter + 1, streamState = Streaming queue})
          )
          cfg
          step

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
