-- | VHtml types and utilities for Lucid HTML with effectful capabilities
module Vira.App.VHtml where

import Data.Binary.Builder (toLazyByteString)
import Effectful (Eff, type (:>))
import Effectful.Reader.Dynamic (ask, asks)
import Effectful.Reader.Dynamic qualified as Reader
import Lucid.Base (Html, HtmlT, ToHtml (toHtmlRaw))
import Lucid.Base qualified as Lucid
import Servant.Links (Link, URI, linkURI)
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

-- | Helper to get a Link for a LinkTo (for hxPostSafe_ and similar)
linkToLink :: (Reader.Reader AppState :> es) => LinkTo -> Eff es Link
linkToLink linkToValue = do
  linkToFn <- asks @AppState linkTo
  pure $ linkToFn linkToValue

-- | Helper to get a URI for a LinkTo
linkToUri :: (Reader.Reader AppState :> es) => LinkTo -> Eff es URI
linkToUri linkToValue = do
  link <- linkToLink linkToValue
  pure $ linkURI link

-- | Helper to get a URL string for a LinkTo (for href attributes)
linkToUrl :: (Reader.Reader AppState :> es) => LinkTo -> Eff es Text
linkToUrl linkToValue = do
  uri <- linkToUri linkToValue
  pure $ toText $ show @String $ uri
