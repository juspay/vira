-- | Utilities for working with haskell-servant
module Vira.App.Servant where

import Lucid (Html, renderBST)
import Network.HTTP.Media qualified as M
import Servant.API.ContentTypes (Accept (contentTypes), MimeRender (mimeRender))
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Prelude hiding (Reader, ask, runReader)

{- | Handy operator to compose nested routes

Useful when working with `fieldLink`
-}
(//) :: a -> (a -> b) -> b
x // f = f x

infixl 1 //

{- | Handy operator to compose routes with captures

Useful when working with `fieldLink`
-}
(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip

infixl 2 /:

data HTML deriving stock (Typeable)

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text"
      M.// "html"
      M./: ("charset", "utf-8")
      :| ["text" M.// "html"]

instance MimeRender HTML (Html ()) where
  mimeRender _ = runIdentity . renderBST

instance MimeRender HTML Text where
  mimeRender _ = fromStrict . encodeUtf8

{- | Transform the monad of a SourceT

XXX: Why is this not in upstream?
-}
mapSourceT :: (Monad m, Monad n) => (forall x. m x -> n x) -> SourceT m a -> SourceT n a
mapSourceT f source = S.fromStepT go
  where
    go = S.Effect $ do
      step <- f $ S.unSourceT source pure
      pure $ transformStep step
    transformStep = \case
      S.Stop -> S.Stop
      S.Error e -> S.Error e
      S.Skip next -> S.Skip (transformStep next)
      S.Yield a next -> S.Yield a (transformStep next)
      S.Effect eff -> S.Effect $ do
        nextStep <- f eff
        pure $ transformStep nextStep
