{-# LANGUAGE TemplateHaskell #-}

{- | Interactive Attic login helper

Provides an interactive script for securely logging into Attic cache servers.
-}
module Attic.Interactive (
  atticLoginInteractiveBin,
) where

import System.Which (staticWhich)

{- | Path to the @attic-login-interactive@ script

This script performs interactive Attic login by:

1. Taking server name and endpoint URL as command-line arguments
2. Prompting for the authentication token (hidden input)
3. Running @attic login@ with the provided credentials

Usage:

@
attic-login-interactive \<server-name\> \<endpoint\>
@

Example:

@
attic-login-interactive cache-example-com https://cache.example.com
@

The script will then prompt for the authentication token securely.
-}
atticLoginInteractiveBin :: FilePath
atticLoginInteractiveBin = $(staticWhich "attic-login-interactive")
