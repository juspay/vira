{- |
Entity-scoped Server-Sent Events for automatic page refresh.

Bridges breadcrumbs to broadcast events: extracts entity scope from page hierarchy,
subscribes to updates, triggers page reload when matching events occur.

Flow: breadcrumbs → 'sseScope' → 'viewStreamScoped' → SSE → 'streamRouteHandler' → reload
-}
module Vira.Web.Stream.ScopedRefresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStreamScoped,
  sseScope,
) where

import Colog.Core (Severity (Debug, Info))
import Data.Text qualified as Text
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API (QueryParam, SourceIO, type (:>))
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import System.FilePattern ((?==))
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (BroadcastScope (..))
import Vira.App.Stack (AppStack)
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = QueryParam "events" Text :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO ScopedRefresh))

-- A scoped refresh signal sent from server to client
newtype ScopedRefresh = ScopedRefresh BroadcastScope

instance ToServerEvent ScopedRefresh where
  toServerEvent (ScopedRefresh _) =
    ServerEvent
      Nothing -- No event type = defaults to "message"
      Nothing -- Event ID
      "location.reload()"

-- | Extract SSE event patterns from breadcrumbs
sseScope :: [LinkTo] -> Maybe [Text]
sseScope crumbs = Just $ case reverse crumbs of
  (Job jobId : _) -> ["job/" <> show @Text jobId]
  (RepoBranch repoName _ : _) -> ["repo/" <> toText repoName <> "/*"]
  (Repo repoName : _) -> ["repo/" <> toText repoName <> "/*"]
  [] -> ["job/*"] -- Index page: subscribe to all job events
  _ -> [] -- No refresh for other pages

-- | SSE listener with event pattern filtering
viewStreamScoped :: [Text] -> AppHtml ()
viewStreamScoped patterns = do
  refreshUrl <- lift $ getLinkUrl LinkTo.Refresh
  let patternsParam = Text.intercalate "," patterns
      link = refreshUrl <> "?events=" <> patternsParam
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    -- Listen for "message" events (default SSE event type) - server filters and sends only matching ones
    script_ [hxSseSwap_ "message"] ("" :: Text)

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) ScopedRefresh
streamRouteHandler mEventPatterns = S.fromStepT $ S.Effect $ do
  let patterns = parseEventPatterns $ fromMaybe "*" mEventPatterns
  tagStreamThread
  log Debug $ "Starting stream with patterns: " <> show patterns
  step 0 patterns <$> Broadcast.subscribeToBroadcasts
  where
    step (n :: Int) patterns chan = S.Effect $ do
      scopes <- Broadcast.consumeBroadcasts chan
      -- Filter scopes by patterns
      let matching = filter (matchesAnyPattern patterns) scopes
      unless (null scopes) $ do
        log Info $ "Received scopes: " <> show scopes <> ", patterns: " <> show patterns
      unless (null matching) $ do
        log Info $ "Filtered scopes: " <> show matching <> " events; n=" <> show n
      -- Send multiple events, one per scope (allows HTMX to filter by event name)
      pure $ yieldAll matching $ step (n + 1) patterns chan

    yieldAll [] next = next
    yieldAll (scope : rest) next =
      S.Yield (ScopedRefresh scope) (yieldAll rest next)

-- | Parse comma-separated event patterns
parseEventPatterns :: Text -> [Text]
parseEventPatterns = map Text.strip . Text.splitOn ","

-- | Check if a scope matches any of the patterns (using filepattern globbing)
matchesAnyPattern :: [Text] -> BroadcastScope -> Bool
matchesAnyPattern patterns scope =
  let scopeStr = show scope
   in any (\pat -> toString pat ?== scopeStr) patterns
