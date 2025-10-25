{-# LANGUAGE OverloadedRecordDot #-}

{- |
Entity-scoped Server-Sent Events for automatic page refresh.

Bridges breadcrumbs to broadcast events: extracts entity scope from page hierarchy,
subscribes to updates, triggers page reload when matching events occur.

Flow: breadcrumbs → 'pageScopePatterns' → 'viewStreamScoped' → SSE → 'streamRouteHandler' → reload
-}
module Vira.Web.Stream.ScopedRefresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStreamScoped,
  pageScopePatterns,
) where

import Colog.Core (Severity (Debug, Error))
import Data.Aeson (encode)
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import Lucid
import Servant.API (QueryParam, SourceIO, type (:>))
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (BroadcastScope (..), UpdateBroadcast, matchesAnyScope, parseScopes)
import Vira.App.Stack (AppStack)
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = QueryParam "events" Text :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO (KeepAlive ScopedRefresh)))

{- | A scoped refresh signal sent from server to client

Wrapped in @KeepAlive@ to add heartbeat support, preventing connection
timeout during long-running operations (e.g., builds taking 5+ minutes).
-}
data ScopedRefresh = ScopedRefresh

instance ToServerEvent ScopedRefresh where
  toServerEvent ScopedRefresh =
    ServerEvent
      (Just "refresh") -- Custom event type to distinguish from heartbeat "message" events
      Nothing -- Event ID
      "Refresh"

-- | `BroadcastScope` patterns monitored by the current page, derived from breadcrumbs
pageScopePatterns :: [LinkTo] -> Maybe (NonEmpty BroadcastScope)
pageScopePatterns crumbs = case reverse crumbs of
  (Job jobId : _) -> Just $ JobScope (Just jobId) :| []
  (RepoBranch repoName _ : _) -> Just $ RepoScope repoName :| []
  (Repo repoName : _) -> Just $ RepoScope repoName :| []
  [] -> Just $ JobScope Nothing :| [] -- Index page: subscribe to all job events
  _ -> Nothing -- No refresh for other pages

-- | SSE listener with event pattern filtering
viewStreamScoped :: NonEmpty BroadcastScope -> AppHtml ()
viewStreamScoped patterns = do
  let patternsParam = decodeUtf8 (encode $ toList patterns)
  link <- lift $ getLinkUrl $ LinkTo.Refresh (Just patternsParam)
  -- Use native EventSource for reliable refresh-triggered reloads
  script_ $
    "new EventSource('" <> link <> "').addEventListener('refresh', () => location.reload());"

data StreamConfig = StreamConfig
  { counter :: Int
  , patterns :: [BroadcastScope]
  , channel :: UpdateBroadcast
  }

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) (KeepAlive ScopedRefresh)
streamRouteHandler mEventPatterns = S.fromStepT $ S.Effect $ do
  tagStreamThread
  patterns <- case parseScopes $ fromMaybe "[]" mEventPatterns of
    Left err -> do
      log Error $ "Invalid broadcast scope patterns: " <> err
      pure [] -- Fall back to empty patterns (no refresh) when parse fails
    Right pats -> pure pats
  log Debug $ "Starting stream with patterns: " <> show patterns
  chan <- Broadcast.subscribeToBroadcasts
  pure $ step StreamConfig {counter = 0, patterns, channel = chan}
  where
    step cfg =
      KeepAlive.withKeepAlive
        (Broadcast.consumeBroadcasts cfg.channel)
        ( \scopes -> do
            -- Filter scopes by patterns (consumeBroadcasts already debounces)
            let matching = filter (matchesAnyScope cfg.patterns) (toList scopes)
            if null matching
              then do
                log Debug $ "No matching events, skipping refresh; n=" <> show cfg.counter
                pure Nothing
              else do
                log Debug $ "Sending refresh for " <> show (length matching) <> " matching events; n=" <> show cfg.counter
                pure $ Just (ScopedRefresh, cfg {counter = cfg.counter + 1})
        )
        cfg
        step
