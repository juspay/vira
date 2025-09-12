{-# LANGUAGE TemplateHaskell #-}

-- | Working with Omnix
module Vira.Lib.Omnix where

import System.Process (CreateProcess, proc)
import System.Which (staticWhich)

{- | Path to the `omnix` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
omnixBin :: FilePath
omnixBin = $(staticWhich "om")

omnixCiProcess :: CreateProcess
omnixCiProcess = omnixCiProcessWithArgs []

omnixCiProcessWithArgs :: [String] -> CreateProcess
omnixCiProcessWithArgs extraArgs =
  proc omnixBin (["ci", "run", "-d", "--"] <> extraArgs)
