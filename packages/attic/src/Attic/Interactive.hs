{-# LANGUAGE TemplateHaskell #-}

{- | Interactive helper scripts for Attic

Provides paths to interactive wrapper scripts that simplify Attic setup
by prompting users for required inputs rather than requiring manual
editing of multi-line commands.
-}
module Attic.Interactive (
  atticLoginInteractiveBin,
) where

import System.Which (staticWhich)

{- | Path to the @attic-login-interactive@ script

Interactive wrapper for attic login that prompts for:
- Server name
- Server endpoint
- Authentication token

This provides better UX than the 3-line copy-paste approach.
-}
atticLoginInteractiveBin :: FilePath
atticLoginInteractiveBin = $(staticWhich "attic-login-interactive")
