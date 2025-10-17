{-# LANGUAGE RecordWildCards #-}

-- | Tool definitions and data operations
module Vira.Tool.Core (
  -- Re-export types
  module Vira.Tool.Type.ToolData,
  module Vira.Tool.Type.Tools,
  ToolError (..),
  -- Operations
  newToolsTVar,
  getTools,
  refreshTools,
  getAllTools,
) where

import Control.Concurrent.STM qualified as STM
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Tools.Cachix qualified as CachixTool
import Vira.Tool.Tools.Git qualified as GitTool
import Vira.Tool.Tools.GitHub qualified as GitHubTool
import Vira.Tool.Type.ToolData
import Vira.Tool.Type.Tools
import Prelude hiding (Reader)

-- | Tool-related errors
newtype ToolError = ToolError Text
  deriving stock (Show)

-- | Create a new TVar with all tools data
newToolsTVar :: (IOE :> es) => Eff es (STM.TVar Tools)
newToolsTVar = do
  initialTools <- getAllTools
  liftIO $ STM.newTVarIO initialTools

-- | Get cached tools from ViraRuntimeState
getTools :: (IOE :> es, Reader.Reader ViraRuntimeState :> es) => Eff es Tools
getTools = do
  ViraRuntimeState {tools = toolsVar} <- Reader.ask
  liftIO $ STM.readTVarIO toolsVar

-- | Refresh tools data and update cache in ViraRuntimeState
refreshTools :: (IOE :> es, Reader.Reader ViraRuntimeState :> es) => Eff es Tools
refreshTools = do
  ViraRuntimeState {tools = toolsVar} <- Reader.ask
  freshTools <- getAllTools
  liftIO $ STM.atomically $ STM.writeTVar toolsVar freshTools
  pure freshTools

-- | Read all tools with metadata and runtime info
getAllTools :: (IOE :> es) => Eff es Tools
getAllTools = do
  attic <- AtticTool.getToolData
  github <- GitHubTool.getToolData
  git <- GitTool.getToolData
  cachix <- CachixTool.getToolData
  pure Tools {..}
