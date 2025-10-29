module Vira.CI.Error (
  ConfigurationError (..),
  PipelineError (..),
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
  | -- | A process in the pipeline failed with non-zero exit code
    PipelineProcessFailed ExitCode
  | -- | devour-flake produced malformed JSON output
    DevourFlakeMalformedOutput FilePath String

-- | Configuration error types
data ConfigurationError
  = InterpreterError InterpreterError
  | MalformedConfig Text
  deriving stock (Show)

instance TS.Show PipelineError where
  show (PipelineToolError (ToolError msg)) =
    "Tool: " <> toString msg
  show (PipelineConfigurationError (InterpreterError herr)) =
    "vira.hs error: " <> case herr of
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
