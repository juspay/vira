-- | Real-time status of the Vira system.
module Vira.Web.Stream.ScopedRefresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStreamScoped,
  sseScope,
) where

import Colog.Core (Severity (Debug))
import Effectful (Eff)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API (SourceIO)
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (BroadcastScope (..))
import Vira.App.Stack (AppStack)
import Vira.Lib.Logging (log, tagCurrentThread)
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO ScopedRefresh))

-- A scoped refresh signal sent from server to client
newtype ScopedRefresh = ScopedRefresh BroadcastScope

instance ToServerEvent ScopedRefresh where
  toServerEvent (ScopedRefresh scope) =
    ServerEvent
      (Just $ show scope)
      Nothing
      "location.reload()"

-- | Extract SSE event scope from breadcrumbs (rightmost entity wins)
sseScope :: [LinkTo] -> Maybe BroadcastScope
sseScope crumbs = case reverse crumbs of
  (Job jobId : _) -> Just $ JobScope jobId
  (RepoBranch repoName _ : _) -> Just $ RepoScope repoName
  (Repo repoName : _) -> Just $ RepoScope repoName
  _ -> Nothing

-- | SSE listener scoped to specific entity (e.g., "repo:my-repo", "job:123")
viewStreamScoped :: BroadcastScope -> AppHtml ()
viewStreamScoped scope = do
  link <- lift $ getLinkUrl LinkTo.Refresh
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    script_ [hxSseSwap_ (show scope)] ("" :: Text)

streamRouteHandler :: (HasCallStack) => SourceT (Eff AppStack) ScopedRefresh
streamRouteHandler = S.fromStepT $ S.Effect $ do
  tagCurrentThread "🐬"
  log Debug "Starting stream"
  step 0 <$> Broadcast.subscribeToBroadcasts
  where
    step (n :: Int) chan = S.Effect $ do
      scopes <- Broadcast.consumeBroadcasts chan
      log Debug $ "Triggering refresh for scopes: " <> show scopes <> "; n=" <> show n
      -- Send multiple events, one per scope (allows HTMX to filter by event name)
      pure $ yieldAll scopes $ step (n + 1) chan

    yieldAll [] next = next
    yieldAll (scope : rest) next =
      S.Yield (ScopedRefresh scope) (yieldAll rest next)
