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

import Colog.Core (Severity (Debug))
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
import System.FilePath ((</>))
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (ScopePattern, matchesAnyPattern, parseScopePatterns)
import Vira.App.Stack (AppStack)
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = QueryParam "events" Text :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO ScopedRefresh))

-- A scoped refresh signal sent from server to client
data ScopedRefresh = ScopedRefresh

instance ToServerEvent ScopedRefresh where
  toServerEvent ScopedRefresh =
    ServerEvent
      Nothing -- No event type = defaults to "message"
      Nothing -- Event ID
      "location.reload()"

-- | Extract SSE event patterns from breadcrumbs
sseScope :: [LinkTo] -> Maybe [ScopePattern]
sseScope crumbs = Just $ case reverse crumbs of
  (Job jobId : _) -> ["job" </> show jobId]
  (RepoBranch repoName _ : _) -> ["repo" </> toString repoName </> "*"]
  (Repo repoName : _) -> ["repo" </> toString repoName </> "*"]
  [] -> ["job" </> "*"] -- Index page: subscribe to all job events
  _ -> [] -- No refresh for other pages

-- | SSE listener with event pattern filtering
viewStreamScoped :: [ScopePattern] -> AppHtml ()
viewStreamScoped patterns = do
  let patternsParam = Text.intercalate "," (map toText patterns)
  link <- lift $ getLinkUrl $ LinkTo.Refresh (Just patternsParam)
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    -- Listen for "message" events (default SSE event type) - server filters and sends only matching ones
    script_ [hxSseSwap_ "message"] ("" :: Text)

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) ScopedRefresh
streamRouteHandler mEventPatterns = S.fromStepT $ S.Effect $ do
  let patterns = parseScopePatterns $ fromMaybe "*" mEventPatterns
  log Debug $ "Starting stream with patterns: " <> show patterns
  tagStreamThread
  step 0 patterns <$> Broadcast.subscribeToBroadcasts
  where
    step (n :: Int) patterns chan = S.Effect $ do
      scopes <- Broadcast.consumeBroadcasts chan
      -- Filter scopes by patterns
      let matching = filter (matchesAnyPattern patterns) scopes
      unless (null matching) $ do
        log Debug $ "Filtered scopes: " <> show matching <> " events; n=" <> show n
      pure $ S.Yield ScopedRefresh $ step (n + 1) patterns chan
