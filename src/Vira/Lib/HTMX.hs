-- | HTMLX utilities
module Vira.Lib.HTMX where

import Lucid
import Lucid.Base

-- | The [hyperscript](https://hyperscript.org/) attribute
hyperscript_ :: Text -> Attributes
hyperscript_ = makeAttributes "_"

hxSseConnect_ :: Text -> Attributes
hxSseConnect_ = makeAttributes "sse-connect"

hxSseSwap_ :: Text -> Attributes
hxSseSwap_ = makeAttributes "sse-swap"
