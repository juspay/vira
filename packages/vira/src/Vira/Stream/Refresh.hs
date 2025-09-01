{-# LANGUAGE RecordWildCards #-}

-- | Real-time status of the Vira system.
module Vira.Stream.Refresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,
  generateSessionId,

  -- * Views
  viewStream,
) where

import Colog.Core (Severity (..))
import Colog.Message (Message, Msg (..))
import Control.Concurrent.STM (TChan, dupTChan, readTChan, tryReadTChan)
import Data.List.NonEmpty qualified as NonEmpty
import Effectful (Eff, (:>))
import Effectful.Colog (Log, logMsg)
import Effectful.Reader.Dynamic (asks)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API (SourceIO)
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Text.Printf (printf)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Stack (AppStack, AppState (nextAvailableID, stateUpdated))
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Refresh))

-- | Generate a unique autoincrementing session ID
generateSessionId :: Eff AppStack Text
generateSessionId = do
  idVar <- asks nextAvailableID
  newId <- liftIO $ atomically $ do
    currentId <- readTVar idVar
    writeTVar idVar (currentId + 1)
    pure currentId
  pure $ show newId

-- | Log with padded stream ID prefix
logWithStreamId :: forall es. (HasCallStack, Log Message :> es) => Text -> Severity -> Text -> Eff es ()
logWithStreamId streamId msgSeverity s = withFrozenCallStack $ do
  let paddedId = toText (printf "%7s" (toString streamId :: String) :: String)
      msgText = "[" <> paddedId <> "] " <> s
  logMsg $ Msg {msgStack = callStack, ..}

-- A status message sent from server to client
--
-- The `Int` is the unique identifier of the status message, which contains the
-- raw HTML of the status.
data Refresh = Refresh

javaScript :: Text
javaScript = "location.reload()"

instance ToServerEvent Refresh where
  toServerEvent = \case
    Refresh ->
      ServerEvent
        (Just "refresh")
        Nothing
        (encodeUtf8 javaScript)

viewStream :: AppHtml ()
viewStream = do
  link <- lift $ getLinkUrl LinkTo.Refresh
  div_ [hxExt_ "sse", hxSseConnect_ link] $ do
    script_ [hxSseSwap_ "refresh"] ("" :: Text)

{- | Drain all items from a TChan (equivalent to CB.drain)
Blocks until at least one item is available, then drains all remaining items
-}
drainTChan :: TChan a -> STM (NonEmpty a)
drainTChan chan = do
  -- Block until first item is available
  firstItem <- readTChan chan
  -- Then drain any remaining items without blocking
  remainingItems <- drainLoop []
  pure $ NonEmpty.fromList (firstItem : reverse remainingItems)
  where
    drainLoop acc = do
      maybeItem <- tryReadTChan chan
      case maybeItem of
        Nothing -> pure acc
        Just item -> drainLoop (item : acc)

-- | Check if state has been updated since the last check (non-blocking)
waitForStateUpdate :: (HasCallStack) => Text -> TChan (Text, ByteString) -> Eff AppStack ()
waitForStateUpdate sessionId chan = do
  events <- liftIO $ atomically $ drainTChan chan
  forM_ events $ \(eventName, _eventData) -> do
    logWithStreamId sessionId Info $ "📝 Update event received: " <> eventName

-- | Drain remaining items from TChan without blocking
drainRemainingTChan :: TChan a -> STM [a]
drainRemainingTChan chan = do
  items <- drainLoop []
  pure $ reverse items
  where
    drainLoop acc = do
      maybeItem <- tryReadTChan chan
      case maybeItem of
        Nothing -> pure acc
        Just item -> drainLoop (item : acc)

streamRouteHandler :: (HasCallStack) => Text -> SourceT (Eff AppStack) Refresh
streamRouteHandler sessionId = S.fromStepT $ S.Effect $ do
  logWithStreamId sessionId Info "🔃 Refresh SSE"
  chan <- asks stateUpdated
  chanDup <- liftIO $ atomically $ dupTChan chan
  -- Drain everything first (non-blocking)
  void $ liftIO $ atomically $ drainRemainingTChan chanDup
  pure $ step 0 chanDup
  where
    step (n :: Int) chan = S.Effect $ do
      waitForStateUpdate sessionId chan
      logWithStreamId sessionId Info $ "🔃 Stream iteration " <> show n
      let refreshMsg = Refresh
      pure $ S.Yield refreshMsg $ step (n + 1) chan
