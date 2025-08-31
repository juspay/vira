-- | HTMX Server-Sent Events utilities for dynamic UI updates
module Vira.App.HTMX.SSE (
  -- * SSE Connection
  sseConnect,
  sseSwap,

  -- * SSE Events
  SSEMessage (..),
  SSERoute,
) where

import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API.EventStream (RecommendedEventSourceHeaders, ServerEvent (..), ServerSentEvents, ToServerEvent (..))
import Servant.API.Stream (SourceIO)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.App.Lucid (AppHtml, getLinkUrl)

{- | Create an SSE connection container that supports "Receiving Multiple Events"

This sets up a single SSE connection that can handle multiple event types.
Child elements should use `sseSwap` to register for specific events.
-}
sseConnect :: LinkTo -> AppHtml () -> AppHtml ()
sseConnect linkTo content = do
  link <- lift $ getLinkUrl linkTo
  div_ [hxExt_ "sse", hxSseConnect_ link] content

{- | Register a child element to swap content on specific SSE events

Use this within an `sseConnect` container to handle specific event types.
-}
sseSwap :: Text -> AppHtml () -> AppHtml ()
sseSwap eventType initialContent = do
  div_ [hxSseSwap_ eventType] initialContent

-- | A Server-Sent Event message containing HTML content
data SSEMessage = SSEMessage
  { eventType :: Text -- Event type for HTMX routing
  , htmlContent :: Html () -- HTML content to swap
  }

instance ToServerEvent SSEMessage where
  toServerEvent (SSEMessage evtType content) =
    ServerEvent
      (Just $ encodeUtf8 evtType)
      Nothing
      (Lucid.renderBS content)

-- | Generic SSE route type for streaming HTML messages
type SSERoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO SSEMessage))
