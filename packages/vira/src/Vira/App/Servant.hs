-- | Utilities for working with haskell-servant
module Vira.App.Servant where

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
