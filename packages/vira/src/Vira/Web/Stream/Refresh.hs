-- | Real-time status of the Vira system.
module Vira.Web.Stream.Refresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
  viewStreamScoped,
) where

import Colog.Core (Severity (Debug))
import Control.Concurrent.STM (TChan, dupTChan)
import Effectful (Eff)
import Effectful.Reader.Dynamic (asks)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API (SourceIO)
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (stateUpdated))
import Vira.Lib.Logging (log, tagCurrentThread)
import Vira.Lib.STM (drainRemainingTChan, drainTChan)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Refresh))

-- A Refresh signal sent from server to client
data Refresh = Refresh | ScopedRefresh Text

instance ToServerEvent Refresh where
  toServerEvent = \case
    Refresh ->
      ServerEvent
        (Just "refresh")
        Nothing
        "location.reload()"
    ScopedRefresh scope ->
      ServerEvent
        (Just $ encodeUtf8 scope)
        Nothing
        "location.reload()"

viewStream :: AppHtml ()
viewStream = do
  link <- lift $ getLinkUrl LinkTo.Refresh
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    script_ [hxSseSwap_ "refresh"] ("" :: Text)

-- | SSE listener scoped to specific entity (e.g., "repo:my-repo", "job:123")
viewStreamScoped :: Text -> AppHtml ()
viewStreamScoped scope = do
  link <- lift $ getLinkUrl LinkTo.Refresh
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    script_ [hxSseSwap_ scope] ("" :: Text)

{- | Check if state has been updated since the last check (non-blocking)
Returns list of event scopes that were broadcast
-}
waitForStateUpdate :: (HasCallStack) => TChan (Text, ByteString) -> Eff AppStack [Text]
waitForStateUpdate chan = do
  events <- liftIO $ atomically $ drainTChan chan
  forM (toList events) $ \(eventName, _eventData) -> do
    log Debug $ "Update event received: " <> eventName
    pure eventName

streamRouteHandler :: (HasCallStack) => SourceT (Eff AppStack) Refresh
streamRouteHandler = S.fromStepT $ S.Effect $ do
  tagCurrentThread "🐬"
  log Debug "Starting stream"
  chan <- asks stateUpdated
  chanDup <- liftIO $ atomically $ do
    dup <- dupTChan chan
    void $ drainRemainingTChan dup
    pure dup
  pure $ step 0 chanDup
  where
    step (n :: Int) chan = S.Effect $ do
      scopes <- waitForStateUpdate chan
      log Debug $ "Triggering refresh for scopes: " <> show scopes <> "; n=" <> show n
      -- Send multiple events, one per scope (allows HTMX to filter by event name)
      pure $ yieldAll scopes $ step (n + 1) chan

    yieldAll [] next = next
    yieldAll (scope : rest) next =
      S.Yield (ScopedRefresh scope) (yieldAll rest next)
