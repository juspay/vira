{-# LANGUAGE OverloadedStrings #-}

{- | Main library module for nix-serve-ng

This library provides a WAI Application for serving a Nix store as a binary cache.

Example usage:

@
import NixServeNg
import qualified Nix
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
    Nix.initStore
    storeDirectory <- Nix.getStoreDir
    let options = ApplicationOptions
            { priority = 30
            , storeDirectory = storeDirectory
            , secretKey = Nothing
            }
    let application = makeApplication options
    Warp.run 5000 application
@
-}
module NixServeNg (
  -- * Application
  makeApplication,
  ApplicationOptions (..),

  -- * Re-exports from Nix module
  Nix.initStore,
  Nix.getStoreDir,
  Nix.PathInfo (..),
  Nix.NoSuchPath (..),
) where

import NixServeNg.Application (ApplicationOptions (..), makeApplication)

import Nix qualified
