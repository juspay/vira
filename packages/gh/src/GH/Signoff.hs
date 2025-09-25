{-# LANGUAGE TemplateHaskell #-}

{- | Working with gh-signoff

A standalone library for gh-signoff operations.
-}
module GH.Signoff (create, Force (..)) where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)
import Prelude hiding (force)

{- | Path to the `gh-signoff` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
ghSignoffBin :: FilePath
ghSignoffBin = $(staticWhich "gh-signoff")

-- | Force flag for signoff operations
data Force = Force | NoForce deriving stock (Show, Eq)

{- | Create a 'CreateProcess' to run @gh-signoff create@

Parameters:
  * force: Whether to use the --force flag
  * name: The signoff name argument
-}
create :: Force -> String -> CreateProcess
create force name =
  proc ghSignoffBin $ "create" : (["--force" | force == Force] <> [name])
