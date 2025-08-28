-- | Server-sent events stream for broadcasting all Vira events
module Vira.Stream.Events (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
) where

import Control.Concurrent (threadDelay)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API
import Servant.API.EventStream
import Servant.Links (linkURI)
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO EventMessage))

-- | SSE message for event broadcasting
data EventMessage = EventMessage
  { eventId :: Int
  , eventType :: Text
  , eventHtml :: Html ()
  }
  deriving stock (Show)

instance ToServerEvent EventMessage where
  toServerEvent (EventMessage ident eventType eventHtml) =
    ServerEvent
      (Just $ encodeUtf8 eventType)
      (Just $ show ident)
      (renderBS eventHtml)

-- | HTMX view for connecting to the event stream
viewStream :: (LinkTo.LinkTo -> Link) -> Html ()
viewStream linkTo = do
  div_ [hxExt_ "sse", hxSseConnect_ link, hxSseSwap_ "job-status"] $ do
    "Loading events..."
  where
    link = show . linkURI $ linkTo LinkTo.EventsGet

-- | SSE handler that broadcasts events from the queue
streamRouteHandler :: App.AppState -> SourceIO EventMessage
streamRouteHandler _cfg = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        liftIO $ threadDelay 1_000_000 -- 1 second between polls

      -- For now, just send heartbeat messages until we implement actual event streaming
      let msg = EventMessage n "heartbeat" (span_ [class_ "text-gray-500"] "♥")
      pure $ S.Yield msg $ step (n + 1)
