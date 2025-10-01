-- | Real-time status of the Vira system.
module Vira.Stream.Refresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
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
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Stack (AppStack, AppState (stateUpdated))
import Vira.Lib.Logging (log, tagCurrentThread)
import Vira.Lib.STM (drainRemainingTChan, drainTChan)
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Refresh))

-- A Refresh signal sent from server to client
data Refresh = Refresh

instance ToServerEvent Refresh where
  toServerEvent = \case
    Refresh ->
      ServerEvent
        (Just "refresh")
        Nothing
        "location.reload()"

viewStream :: AppHtml ()
viewStream = do
  link <- lift $ getLinkUrl LinkTo.Refresh
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    script_ [hxSseSwap_ "refresh"] ("" :: Text)

-- | Check if state has been updated since the last check (non-blocking)
waitForStateUpdate :: (HasCallStack) => TChan (Text, ByteString) -> Eff AppStack ()
waitForStateUpdate chan = do
  events <- liftIO $ atomically $ drainTChan chan
  forM_ events $ \(eventName, _eventData) -> do
    log Debug $ "Update event received: " <> eventName

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
      waitForStateUpdate chan
      log Debug $ "Triggering refresh; n=" <> show n
      pure $ S.Yield Refresh $ step (n + 1) chan
