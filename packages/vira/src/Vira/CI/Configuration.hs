{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Configuration (
  applyConfig,
) where

import Language.Haskell.Hint.Nix
import Language.Haskell.Interpreter (InterpreterError)
import Language.Haskell.Interpreter qualified as Hint
import Vira.CI.Context (ViraContext)
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Apply a Haskell configuration file to modify a 'ViraPipeline'
applyConfig ::
  (MonadIO m) =>
  -- | Contents of Haskell config file
  Text ->
  -- | Current 'ViraContext'
  ViraContext ->
  -- | Default 'ViraPipeline' configuration
  ViraPipeline ->
  m (Either InterpreterError ViraPipeline)
applyConfig configContent ctx pipeline = do
  result <- liftIO $ runInterpreterWithNixPackageDb $ do
    -- Set up the interpreter context
    Hint.set
      [ Hint.languageExtensions
          Hint.:= [ Hint.OverloadedStrings
                  , Hint.OverloadedLists
                  , Hint.MultiWayIf
                  , Hint.UnknownExtension "OverloadedRecordDot"
                  , Hint.UnknownExtension "OverloadedRecordUpdate"
                  , Hint.UnknownExtension "RebindableSyntax"
                  ]
      ]

    -- Import necessary modules
    -- GHC.Exts must be qualified to allow `fromList = E.fromList` binding
    -- for RebindableSyntax + OverloadedLists interaction
    Hint.setImportsQ
      [ -- Provide the user with relude functions
        ("Relude", Nothing)
      , -- For fromList (required by RebindableSyntax + OverloadedLists)
        ("GHC.Exts", Just "E")
      , -- For record update syntax
        ("GHC.Records.Compat", Nothing)
      , -- Vira CI types
        ("Vira.CI.Context", Nothing)
      , ("Vira.CI.Pipeline.Type", Nothing)
      , -- Git types, used by CI types
        ("Effectful.Git", Nothing)
      ]

    -- Wrap the config content with definitions required by RebindableSyntax:
    -- - ifThenElse: for MultiWayIf syntax
    -- - fromList: for OverloadedLists syntax (uses qualified E.fromList)
    let wrappedContent = "let ifThenElse :: Bool -> a -> a -> a; ifThenElse True t _ = t; ifThenElse False _ f = f; fromList :: E.IsList l => [E.Item l] -> l; fromList = E.fromList in " <> configContent

    -- Interpret the configuration code directly as a function
    configFn <- Hint.interpret (toString wrappedContent) (Hint.as :: ViraContext -> ViraPipeline -> ViraPipeline)

    -- Apply the configuration function
    return $ configFn ctx pipeline

  case result of
    Left err -> return $ Left err
    Right modifiedPipeline -> return $ Right modifiedPipeline
