-- | Real-time status of the Vira system.
module Vira.Stream.Refresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.Time.Clock (UTCTime, getCurrentTime)
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

-- | Check if state has been updated since the last check
hasRecentStateUpdate :: Maybe UTCTime -> Eff AppStack Bool
hasRecentStateUpdate lastUpdated = do
  buffer <- asks stateUpdated
  updates <- liftIO $ Control.Concurrent.STM.atomically $ CB.drain buffer
  case updates of
    Nothing -> pure False
    Just updateTimes -> do
      let timesList = toList updateTimes
      case viaNonEmpty last timesList of
        Nothing -> pure False
        Just latestUpdate ->
          case lastUpdated of
            Nothing -> pure True -- First time, so refresh
            Just lastTime -> pure $ latestUpdate > lastTime

streamRouteHandler :: SourceT (Eff AppStack) Status
streamRouteHandler = S.fromStepT $ S.Effect $ do
  putStrLn "🔃 Refresh SSE"
  pure $ step 0 Nothing
  where
    step (n :: Int) lastUpdated = S.Effect $ do
      when (n > 0) $ do
        liftIO $ threadDelay 1_000_000

      -- Check if state has been updated since last refresh
      shouldRefresh <- hasRecentStateUpdate lastUpdated
      putStrLn $ "🔃 Stream iteration " <> show n <> ", shouldRefresh=" <> show shouldRefresh
      if shouldRefresh
        then do
          now <- liftIO getCurrentTime
          let refreshMsg = Refresh "location.reload()"
          pure $ S.Yield refreshMsg $ step (n + 1) (Just now)
        else
          pure $ S.Skip $ step n lastUpdated
