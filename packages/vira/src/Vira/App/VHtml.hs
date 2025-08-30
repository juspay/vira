-- | VHtml types and utilities for Lucid HTML with effectful capabilities
module Vira.App.VHtml where

import Data.Binary.Builder (toLazyByteString)
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

-- | Convert a `VHtml` to a Lucid `Html`
runVHtml :: VHtml () -> Eff AppStack (Html ())
runVHtml htmlT = do
  (builder, _) <- Lucid.runHtmlT htmlT
  pure $ toHtmlRaw $ toLazyByteString builder

-- | Convert VHtml to Html in the Servant effect stack
runVHtmlInServant :: VHtml () -> Eff AppServantStack (Html ())
runVHtmlInServant vhtml = do
  cfg <- ask @AppState
  liftIO $ runApp cfg $ runVHtml vhtml

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
