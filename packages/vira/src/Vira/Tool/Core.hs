{-# LANGUAGE RecordWildCards #-}

-- | Tool definitions and data operations
module Vira.Tool.Core (
  -- Re-export types
  module Vira.Tool.Type,
  -- Operations
  newToolsTVar,
  getTools,
  refreshTools,
) where

import Control.Concurrent.STM qualified as STM
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Stack (AppState (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Tools.Cachix qualified as CachixTool
import Vira.Tool.Tools.Git qualified as GitTool
import Vira.Tool.Tools.GitHub qualified as GitHubTool
import Vira.Tool.Tools.Omnix qualified as OmnixTool
import Vira.Tool.Type
import Prelude hiding (Reader)

-- | Create a new TVar with all tools data
newToolsTVar :: (IOE :> es) => Eff es (STM.TVar Tools)
newToolsTVar = do
  initialTools <- getAllTools
  liftIO $ STM.newTVarIO initialTools

-- | Get cached tools from AppState
getTools :: (IOE :> es, Reader.Reader AppState :> es) => Eff es Tools
getTools = do
  AppState {tools = toolsVar} <- Reader.ask
  liftIO $ STM.readTVarIO toolsVar

-- | Refresh tools data and update cache in AppState
refreshTools :: (IOE :> es, Reader.Reader AppState :> es) => Eff es Tools
refreshTools = do
  AppState {tools = toolsVar} <- Reader.ask
  freshTools <- getAllTools
  liftIO $ STM.atomically $ STM.writeTVar toolsVar freshTools
  pure freshTools

-- | Read all tools with metadata and runtime info
getAllTools :: (IOE :> es) => Eff es Tools
getAllTools = do
  attic <- AtticTool.getToolData
  github <- GitHubTool.getToolData
  omnix <- OmnixTool.getToolData
  git <- GitTool.getToolData
  cachix <- CachixTool.getToolData
  pure Tools {..}
