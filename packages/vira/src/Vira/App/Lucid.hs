-- | AppHtml types and utilities for Lucid HTML with effectful capabilities
module Vira.App.Lucid where

import Effectful (Eff, type (:>))
import Effectful.Reader.Dynamic (asks)
import Effectful.Reader.Dynamic qualified as Reader
import Lucid.Base (Html, HtmlT, ToHtml (toHtmlRaw))
import Lucid.Base qualified as Lucid
import Servant.Links (Link, linkURI)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.App.Stack (AppServantStack)
import Vira.App.Type (ViraRuntimeState (linkTo))
import Prelude hiding (ask, asks)

{- | Like `Html` but can do application effects

Use `lift` to perform effects.
-}
type AppHtml = HtmlT (Eff AppServantStack)

-- | Convert AppHtml to Html in the Servant effect stack
runAppHtml :: AppHtml () -> Eff AppServantStack (Html ())
runAppHtml htmlT = do
  toHtmlRaw <$> Lucid.renderBST htmlT

-- | Get a `Link` to a part of the application
getLink :: (Reader.Reader ViraRuntimeState :> es) => LinkTo -> Eff es Link
getLink linkToValue = do
  linkToFn <- asks @ViraRuntimeState linkTo
  pure $ linkToFn linkToValue

-- | Link `getLink` but as URL text.
getLinkUrl :: (Reader.Reader ViraRuntimeState :> es) => LinkTo -> Eff es Text
getLinkUrl linkToValue = do
  uri <- linkURI <$> getLink linkToValue
  pure $ show @Text $ uri
