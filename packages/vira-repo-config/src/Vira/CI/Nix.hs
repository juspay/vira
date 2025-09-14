{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Nix where

import IncludeEnv.TH
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

-- Embed environment variables at compile time (required)
$(includeEnv "HINT_GHC_LIB_DIR" "ghcLibDir")
ghcLibDir :: String

$(includeEnv "HINT_GHC_PACKAGE_PATH" "ghcPackagePath")
ghcPackagePath :: String

-- | Helper function to run interpreter with the proper package database in Nix
runInterpreterWithNixPackageDb :: InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterWithNixPackageDb =
  unsafeRunInterpreterWithArgsLibdir
    ["-package-db", ghcPackagePath]
    ghcLibDir
