-- | HTMX utilities not yet available in @htmx-lucid@ (candidates for upstreaming)
module Lucid.Htmx.Contrib where

import Htmx.Lucid.Core (hxPost_)
import Lucid
import Lucid.Base
import Servant.API (toUrlPiece)
import Servant.Links (Link)

-- | The [hyperscript](https://hyperscript.org/) attribute (@_@)
hyperscript_ :: Text -> Attributes
hyperscript_ = makeAttributes "_"

-- | HTMX SSE connection attribute (@sse-connect@)
hxSseConnect_ :: Text -> Attributes
hxSseConnect_ = makeAttributes "sse-connect"

-- | HTMX SSE swap attribute (@sse-swap@)
hxSseSwap_ :: Text -> Attributes
hxSseSwap_ = makeAttributes "sse-swap"

-- | HTMX SSE close attribute (@sse-close@)
hxSseClose_ :: Text -> Attributes
hxSseClose_ = makeAttributes "sse-close"

-- | HTMX confirm dialog attribute (@hx-confirm@)
hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "hx-confirm"

{- | Safe version of 'Htmx.Lucid.Core.hxPost_' that works with 'Servant.Links.Link'

Fixed version of functions in @htmx-servant@:
https://github.com/JonathanLorimer/htmx/issues/16
-}
hxPostSafe_ :: Link -> Attributes
hxPostSafe_ = hxPost_ . toUrlPiece
