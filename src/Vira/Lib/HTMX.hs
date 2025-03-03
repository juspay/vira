-- | HTMLX utilities
module Vira.Lib.HTMX where

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

-- Fixed version of functions in htmx-servant
-- https://github.com/JonathanLorimer/htmx/issues/16

hxPostSafe_ :: Link -> Attributes
hxPostSafe_ = hxPost_ . toUrlPiece
