-- | VHtml types and utilities for Lucid HTML with effectful capabilities
module Vira.App.VHtml where

import Data.Binary.Builder (toLazyByteString)
import Effectful (Eff, type (:>))
import Effectful.Reader.Dynamic (ask, asks)
import Effectful.Reader.Dynamic qualified as Reader
import Lucid.Base (Html, HtmlT, ToHtml (toHtmlRaw))
import Lucid.Base qualified as Lucid
import Servant.API.Stream (SourceIO)
import Servant.Links (Link, linkURI)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.LinkTo.Type (LinkTo)
import Vira.App.Stack (AppServantStack, AppStack, AppState (linkTo), runApp)
import Prelude hiding (ask, asks)

{- | Like `Html` but can do application effects

Use `lift` to perform effects.
-}
type VHtml = HtmlT (Eff AppStack)

{- | Like `SourceIO` but can do application effects

Use `lift` to perform effects.
-}
type VSource a = SourceT (Eff AppStack) a

-- | Convert VHtml to Html in the Servant effect stack
runVHtml :: VHtml () -> Eff AppServantStack (Html ())
runVHtml vhtml = do
  cfg <- ask @AppState
  liftIO $ runApp cfg $ runVHtml' vhtml

runVHtml' :: VHtml () -> Eff AppStack (Html ())
runVHtml' htmlT = do
  (builder, _) <- Lucid.runHtmlT htmlT
  pure $ toHtmlRaw $ toLazyByteString builder

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

-- | Convert VSource to SourceIO by running effects in IO
runVSourceIO :: AppState -> VSource a -> SourceIO a
runVSourceIO cfg vsource = S.fromStepT go
  where
    go = S.Effect $ do
      step <- runApp cfg $ S.unSourceT vsource pure
      pure $ transformStep step
    transformStep = \case
      S.Stop -> S.Stop
      S.Error e -> S.Error e
      S.Skip next -> S.Skip (transformStep next)
      S.Yield a next -> S.Yield a (transformStep next)
      S.Effect eff -> S.Effect $ do
        nextStep <- runApp cfg eff
        pure $ transformStep nextStep
