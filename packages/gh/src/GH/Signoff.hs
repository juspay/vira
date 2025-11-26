{-# LANGUAGE TemplateHaskell #-}

{- | Working with gh-signoff

A standalone library for gh-signoff operations.
-}
module GH.Signoff (create, Force (..), ghSignoffBin) where

import Data.List.NonEmpty qualified as NE
import System.Process (CreateProcess, proc)
import System.Which (staticWhich)
import Prelude hiding (force)

{- | Path to the @gh-signoff@ executable

This should be available in the PATH, thanks to Nix and @which@ library.
-}
ghSignoffBin :: FilePath
ghSignoffBin = $(staticWhich "gh-signoff")

-- | Force flag for signoff operations
data Force = Force | NoForce deriving stock (Show, Eq)

{- | Create a 'CreateProcess' to run @gh-signoff create@

Parameters:
  * @force@: Whether to use the @--force@ flag
  * @names@: The signoff name arguments (one or more)
-}
create :: Force -> NonEmpty String -> CreateProcess
create force names =
  proc ghSignoffBin $ "create" : (["-f" | force == Force] <> NE.toList names)
