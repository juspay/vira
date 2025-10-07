module Vira.CI.Error (
  ConfigurationError (..),
  PipelineError (..),
) where

import Data.List qualified
import Language.Haskell.Interpreter (GhcError (..), InterpreterError (..))
import Text.Show qualified as TS
import Vira.Supervisor.Type (Terminated)
import Vira.Tool.Core (ToolError (..))

-- | Pipeline-specific errors
data PipelineError
  = PipelineConfigurationError ConfigurationError
  | PipelineToolError ToolError
  | PipelineTerminated Terminated

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
