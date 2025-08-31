-- | HTMX Server-Sent Events utilities for dynamic UI updates
module Vira.HTMX.SSE (
  -- * SSE Connection
  sseConnect,
  sseSwap,
) where

import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
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
sseSwap eventType content = do
  div_ [hxSseSwap_ eventType] content
