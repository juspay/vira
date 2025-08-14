-- | HTMX utilities not yet available in htmx-lucid (candidates for upstreaming)
module Lucid.Htmx.Contrib where

import Htmx.Lucid.Core (hxPost_)
import Lucid
import Lucid.Base
import Servant.API (toUrlPiece)
import Servant.Links (Link)

-- | The [hyperscript](https://hyperscript.org/) attribute
hyperscript_ :: Text -> Attributes
hyperscript_ = makeAttributes "_"

hxSseConnect_ :: Text -> Attributes
hxSseConnect_ = makeAttributes "sse-connect"

hxSseSwap_ :: Text -> Attributes
hxSseSwap_ = makeAttributes "sse-swap"

hxSseClose_ :: Text -> Attributes
hxSseClose_ = makeAttributes "sse-close"

hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "hx-confirm"

-- Fixed version of functions in htmx-servant
-- https://github.com/JonathanLorimer/htmx/issues/16

hxPostSafe_ :: Link -> Attributes
hxPostSafe_ = hxPost_ . toUrlPiece
