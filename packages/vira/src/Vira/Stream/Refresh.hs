-- | Real-time status of the Vira system.
module Vira.Stream.Refresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
) where

import Control.Concurrent.STM (TChan, dupTChan, readTChan, tryReadTChan)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (UTCTime)
import Effectful (Eff)
import Effectful.Reader.Dynamic (asks)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Stack (AppStack, AppState (stateUpdated))
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Status))

-- A status message sent from server to client
--
-- The `Int` is the unique identifier of the status message, which contains the
-- raw HTML of the status.
newtype Status = Refresh Text

instance ToServerEvent Status where
  toServerEvent = \case
    Refresh js ->
      ServerEvent
        (Just "refresh")
        Nothing
        (encodeUtf8 js)

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
hasRecentStateUpdate :: TChan UTCTime -> Eff AppStack ()
hasRecentStateUpdate chan = do
  void $ liftIO $ atomically $ drainTChan chan

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

streamRouteHandler :: SourceT (Eff AppStack) Status
streamRouteHandler = S.fromStepT $ S.Effect $ do
  putStrLn "🔃 Refresh SSE"
  chan <- asks stateUpdated
  chanDup <- liftIO $ atomically $ dupTChan chan
  -- Drain everything first (non-blocking)
  void $ liftIO $ atomically $ drainRemainingTChan chanDup
  pure $ step 0 chanDup
  where
    step (n :: Int) chan = S.Effect $ do
      -- Check if state has been updated since last refresh
      shouldRefresh <- hasRecentStateUpdate chan
      putStrLn $ "🔃 Stream iteration " <> show n <> ", shouldRefresh=" <> show shouldRefresh
      let refreshMsg = Refresh "location.reload()"
      pure $ S.Yield refreshMsg $ step (n + 1) chan
