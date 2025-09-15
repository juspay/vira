{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.RepoConfig (
  applyConfig,
  applyConfigFromFile,
) where

import Language.Haskell.Interpreter qualified as Hint
import System.Directory (doesFileExist)
import Vira.CI.Environment.Type (ViraEnvironment)
import Vira.CI.Nix
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Apply a Haskell configuration file to modify a pipeline
applyConfig ::
  -- | Contents of Haskell config file
  Text ->
  -- | Current environment context
  ViraEnvironment ->
  -- | Default pipeline configuration
  ViraPipeline ->
  IO (Either String ViraPipeline)
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
    Left err -> return $ Left (show err)
    Right modifiedPipeline -> return $ Right modifiedPipeline

-- | Apply configuration from a file path
applyConfigFromFile ::
  -- | Path to Haskell config file
  FilePath ->
  -- | Current environment context
  ViraEnvironment ->
  -- | Default pipeline configuration
  ViraPipeline ->
  IO (Either String ViraPipeline)
applyConfigFromFile configPath env pipeline = do
  exists <- doesFileExist configPath
  if not exists
    then return $ Left ("Config file not found: " <> configPath)
    else do
      content <- readFileBS configPath
      applyConfig (decodeUtf8 content) env pipeline
