{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.RepoConfig (
  applyConfig,
  applyConfigFromFile,
  ConfigError (..),
) where

import Language.Haskell.Interpreter qualified as Hint
import System.Directory (doesFileExist)
import Vira.CI.Environment.Type (ViraEnvironment)
import Vira.CI.Nix
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Errors that can occur during configuration application
data ConfigError
  = ConfigFileNotFound FilePath
  | ConfigSyntaxError String
  | ConfigRuntimeError String
  | ConfigMissingFunction String
  deriving (Show, Eq)

-- | Apply a Haskell configuration file to modify a pipeline
applyConfig ::
  -- | Contents of Haskell config file
  Text ->
  -- | Current environment context
  ViraEnvironment ->
  -- | Default pipeline configuration
  ViraPipeline ->
  IO (Either ConfigError ViraPipeline)
applyConfig configContent env pipeline = do
  result <- runInterpreterWithNixPackageDb $ do
    -- Set up the interpreter context
    Hint.set [Hint.languageExtensions Hint.:= [Hint.OverloadedStrings]]

    -- Import necessary modules
    Hint.setImports
      [ "Prelude"
      , "Data.Text"
      , "Vira.CI.Environment.Type"
      , "Vira.CI.Pipeline.Type"
      , "Optics.Core"
      ]

    -- Load the configuration code as a let binding
    Hint.runStmt ("let " <> toString configContent)

    -- Look for the configureVira function
    configFn <- Hint.interpret "configureVira" (Hint.as :: ViraEnvironment -> ViraPipeline -> ViraPipeline)

    -- Apply the configuration function
    return $ configFn env pipeline

  case result of
    Left err -> return $ Left (interpretError err)
    Right modifiedPipeline -> return $ Right modifiedPipeline

-- | Convert Hint errors to our ConfigError type
interpretError :: Hint.InterpreterError -> ConfigError
interpretError err = case err of
  Hint.UnknownError msg -> ConfigRuntimeError msg
  Hint.WontCompile errs -> ConfigSyntaxError (toString $ unlines $ map (toText . Hint.errMsg) errs)
  Hint.NotAllowed msg -> ConfigRuntimeError msg
  Hint.GhcException msg -> ConfigRuntimeError msg

-- | Apply configuration from a file path
applyConfigFromFile ::
  -- | Path to Haskell config file
  FilePath ->
  -- | Current environment context
  ViraEnvironment ->
  -- | Default pipeline configuration
  ViraPipeline ->
  IO (Either ConfigError ViraPipeline)
applyConfigFromFile configPath env pipeline = do
  exists <- doesFileExist configPath
  if not exists
    then return $ Left (ConfigFileNotFound configPath)
    else do
      content <- readFileText configPath
      applyConfig content env pipeline
