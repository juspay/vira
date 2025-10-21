{-# LANGUAGE RecordWildCards #-}

-- | Tool definitions and data operations
module Vira.Environment.Tool.Core (
  -- Re-export types
  module Vira.Environment.Tool.Type.ToolData,
  module Vira.Environment.Tool.Type.Tools,
  ToolError (..),
  -- Operations
  newToolsTVar,
  getTools,
  refreshTools,
  getAllTools,
) where

import Control.Concurrent.STM qualified as STM
import Effectful (Eff, IOE, (:>))
import Effectful.Process (Process)
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Environment.Tool.Tools.Attic qualified as AtticTool
import Vira.Environment.Tool.Tools.Cachix qualified as CachixTool
import Vira.Environment.Tool.Tools.Git qualified as GitTool
import Vira.Environment.Tool.Tools.GitHub qualified as GitHubTool
import Vira.Environment.Tool.Tools.Nix qualified as NixTool
import Vira.Environment.Tool.Type.ToolData
import Vira.Environment.Tool.Type.Tools
import Prelude hiding (Reader)

-- | Tool-related errors
newtype ToolError = ToolError Text
  deriving stock (Show)

-- | Create a new TVar with all tools data
newToolsTVar :: (Process :> es, IOE :> es) => Eff es (STM.TVar Tools)
newToolsTVar = do
  initialTools <- getAllTools
  liftIO $ STM.newTVarIO initialTools

-- | Get cached tools from ViraRuntimeState
getTools :: (IOE :> es, Reader.Reader ViraRuntimeState :> es) => Eff es Tools
getTools = do
  ViraRuntimeState {tools = toolsVar} <- Reader.ask
  liftIO $ STM.readTVarIO toolsVar

-- | Refresh tools data and update cache in ViraRuntimeState
refreshTools :: (Process :> es, IOE :> es, Reader.Reader ViraRuntimeState :> es) => Eff es Tools
refreshTools = do
  ViraRuntimeState {tools = toolsVar} <- Reader.ask
  freshTools <- getAllTools
  liftIO $ STM.atomically $ STM.writeTVar toolsVar freshTools
  pure freshTools

-- | Read all tools with metadata and runtime info
getAllTools :: (Process :> es, IOE :> es) => Eff es Tools
getAllTools = do
  attic <- AtticTool.getToolData
  github <- GitHubTool.getToolData
  git <- GitTool.getToolData
  cachix <- CachixTool.getToolData
  nix <- NixTool.getToolData
  pure Tools {..}
