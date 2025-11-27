module Vira.CI.Error (
  ConfigurationError (..),
  PipelineError (..),
  pipelineToolError,
  -- Re-export ToolError but only for pattern matching
  ToolError (..),
) where

import Data.List qualified
import Language.Haskell.Interpreter (GhcError (..), InterpreterError (..))
import System.Exit (ExitCode)
import Text.Show qualified as TS
import Vira.Environment.Tool.Core (ToolError (..))
import Vira.Supervisor.Type (Terminated)

-- | Pipeline-specific errors
data PipelineError
  = PipelineConfigurationError ConfigurationError
  | PipelineToolError ToolError
  | PipelineTerminated Terminated
  | -- | A process in the pipeline failed with non-zero 'ExitCode'
    PipelineProcessFailed ExitCode
  | -- | @devour-flake@ produced malformed JSON output
    DevourFlakeMalformedOutput FilePath String

{- | Configuration error types

ONLY for @vira.hs@ file parsing/interpretation errors.
For tool configuration errors (attic, git, platform detection, etc),
use 'PipelineToolError' with 'ToolError' instead.
-}
data ConfigurationError
  = InterpreterError InterpreterError
  | MalformedConfig Text
  deriving stock (Show)

{- | Convert tool errors to PipelineError with optional suggestion.

Use this for tool/platform failures. Include suggestions when we can provide
actionable commands to fix the issue (e.g., config setup). Use Nothing for
errors beyond user control (e.g., HTTP failures).

The suggestion is automatically converted to Text via Show.
-}
pipelineToolError :: (Show suggestion) => Text -> Maybe suggestion -> PipelineError
pipelineToolError errorMsg maybeSuggestion =
  PipelineToolError (ToolError {message = errorMsg, suggestion = show @Text <$> maybeSuggestion})

instance TS.Show PipelineError where
  show (PipelineToolError (ToolError msg sug)) =
    let suggestionText = case sug of
          Nothing -> ""
          Just s -> "\n\nSuggestion: Run the following in your terminal\n\n" <> s
     in "Tool error: " <> toString (msg <> suggestionText)
  show (PipelineConfigurationError (InterpreterError herr)) =
    "vira.hs interpreter error: " <> case herr of
      WontCompile ghcErrors -> "WontCompile\n" <> Data.List.unlines (errMsg <$> ghcErrors)
      UnknownError err -> "UnknownError\n" <> err
      NotAllowed err -> "NotAllowed\n" <> err
      GhcException err -> "GhcException\n" <> err
  show (PipelineConfigurationError (MalformedConfig msg)) =
    "vira.hs has malformed config: " <> toString msg
  show (PipelineTerminated err) = displayException err
  show (PipelineProcessFailed exitCode) = "Process failed: " <> show exitCode
  show (DevourFlakeMalformedOutput path err) =
    "devour-flake produced malformed JSON at '" <> path <> "':\n" <> err
