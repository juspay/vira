{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Configuration (
  applyConfig,
) where

import Language.Haskell.Hint.Nix
import Language.Haskell.Interpreter (InterpreterError)
import Language.Haskell.Interpreter qualified as Hint
import Vira.CI.Environment.Type (ViraEnvironment)
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Apply a Haskell configuration file to modify a pipeline
applyConfig ::
  -- | Contents of Haskell config file
  Text ->
  -- | Current environment context
  ViraEnvironment ->
  -- | Default pipeline configuration
  ViraPipeline ->
  IO (Either InterpreterError ViraPipeline)
applyConfig configContent env pipeline = do
  result <- runInterpreterWithNixPackageDb $ do
    -- Set up the interpreter context
    Hint.set [Hint.languageExtensions Hint.:= [Hint.OverloadedStrings, Hint.UnknownExtension "OverloadedRecordDot", Hint.UnknownExtension "OverloadedLabels"]]

    -- Import necessary modules
    Hint.setImports
      [ "Prelude"
      , "Data.Text"
      , "Vira.CI.Environment.Type"
      , "Vira.CI.Pipeline.Type"
      , "Vira.State.Type"
      , "Effectful.Git"
      , "Optics.Core"
      ]

    -- Interpret the configuration code directly as a function
    configFn <- Hint.interpret (toString configContent) (Hint.as :: ViraEnvironment -> ViraPipeline -> ViraPipeline)

    -- Apply the configuration function
    return $ configFn env pipeline

  case result of
    Left err -> return $ Left err
    Right modifiedPipeline -> return $ Right modifiedPipeline
