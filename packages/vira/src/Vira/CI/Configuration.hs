{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Configuration (
  applyConfig,
) where

import Language.Haskell.Hint.Nix
import Language.Haskell.Interpreter (InterpreterError)
import Language.Haskell.Interpreter qualified as Hint
import Vira.CI.Context (ViraContext)
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Apply a Haskell configuration file to modify a pipeline
applyConfig ::
  (MonadIO m) =>
  -- | Contents of Haskell config file
  Text ->
  -- | Current context
  ViraContext ->
  -- | Default pipeline configuration
  ViraPipeline ->
  m (Either InterpreterError ViraPipeline)
applyConfig configContent ctx pipeline = do
  result <- liftIO $ runInterpreterWithNixPackageDb $ do
    -- Set up the interpreter context
    Hint.set
      [ Hint.languageExtensions
          Hint.:= [ Hint.OverloadedStrings
                  , Hint.UnknownExtension "OverloadedRecordDot"
                  , Hint.UnknownExtension "OverloadedRecordUpdate"
                  , Hint.UnknownExtension "RebindableSyntax"
                  ]
      ]

    -- Import necessary modules
    Hint.setImports
      [ "Relude"
      , "Data.Text"
      , "Vira.CI.Context"
      , "Vira.CI.Pipeline.Type"
      , "Effectful.Git"
      , "GHC.Records.Compat"
      ]

    -- Wrap the config content with ifThenElse definition for RebindableSyntax
    -- RebindableSyntax requires ifThenElse to be in scope
    let wrappedContent = "let ifThenElse :: Bool -> a -> a -> a; ifThenElse True t _ = t; ifThenElse False _ f = f in " <> configContent

    -- Interpret the configuration code directly as a function
    configFn <- Hint.interpret (toString wrappedContent) (Hint.as :: ViraContext -> ViraPipeline -> ViraPipeline)

    -- Apply the configuration function
    return $ configFn ctx pipeline

  case result of
    Left err -> return $ Left err
    Right modifiedPipeline -> return $ Right modifiedPipeline
