-- | Utilities for working with haskell-servant
module Vira.App.Servant where

import Effectful (Eff)
import Servant.API.Stream (SourceIO)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.Stack (AppStack, AppState, runApp)
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

{- | Like `SourceIO` but can do application effects

Use `lift` to perform effects.
-}
type VSource a = SourceT (Eff AppStack) a

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
