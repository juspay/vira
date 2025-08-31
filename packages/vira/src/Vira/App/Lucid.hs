-- | VHtml types and utilities for Lucid HTML with effectful capabilities
module Vira.App.Lucid where

import Effectful (Eff, type (:>))
import Effectful.Reader.Dynamic (ask, asks)
import Effectful.Reader.Dynamic qualified as Reader
import Lucid.Base (Html, HtmlT, ToHtml (toHtmlRaw))
import Lucid.Base qualified as Lucid
import Servant.Links (Link, linkURI)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.App.Stack (AppServantStack, AppStack, AppState (linkTo), runApp)
import Prelude hiding (ask, asks)

{- | Like `Html` but can do application effects

Use `lift` to perform effects.
-}
type VHtml = HtmlT (Eff AppStack)

-- | Convert VHtml to Html in the Servant effect stack
runVHtml :: VHtml () -> Eff AppServantStack (Html ())
runVHtml vhtml = do
  cfg <- ask @AppState
  liftIO $ runApp cfg $ runVHtml' vhtml

runVHtml' :: VHtml () -> Eff AppStack (Html ())
runVHtml' htmlT = do
  toHtmlRaw <$> Lucid.renderBST htmlT

-- | Get a `Link` to a part of the application
getLink :: (Reader.Reader AppState :> es) => LinkTo -> Eff es Link
getLink linkToValue = do
  linkToFn <- asks @AppState linkTo
  pure $ linkToFn linkToValue

-- | Link `getLink` but as URL text.
getLinkUrl :: (Reader.Reader AppState :> es) => LinkTo -> Eff es Text
getLinkUrl linkToValue = do
  uri <- linkURI <$> getLink linkToValue
  pure $ show @Text $ uri
