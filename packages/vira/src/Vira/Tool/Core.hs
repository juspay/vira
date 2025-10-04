-- | Tool definitions and data operations
module Vira.Tool.Core (
  -- Re-export types
  module Vira.Tool.Type,
  -- Operations
  newToolsTVar,
  getTools,
  refreshTools,
) where

import Attic qualified
import Attic.Config qualified
import Control.Concurrent.STM qualified as STM
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Some (Some (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Git qualified as Git
import Effectful.Reader.Dynamic qualified as Reader
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Vira.App.Stack (AppState (..))
import Vira.Lib.Cachix qualified as Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.Tool.Type
import Prelude hiding (Reader)

-- | Create a new TVar with all tools data
newToolsTVar :: (IOE :> es) => Eff es (STM.TVar (DMap Tool ToolData))
newToolsTVar = do
  initialTools <- getAllToolData
  liftIO $ STM.newTVarIO initialTools

-- | Get cached tools from AppState
getTools :: (IOE :> es, Reader.Reader AppState :> es) => Eff es (DMap Tool ToolData)
getTools = do
  AppState {tools = toolsVar} <- Reader.ask
  liftIO $ STM.readTVarIO toolsVar

-- | Refresh tools data and update cache in AppState
refreshTools :: (IOE :> es, Reader.Reader AppState :> es) => Eff es (DMap Tool ToolData)
refreshTools = do
  AppState {tools = toolsVar} <- Reader.ask
  freshTools <- getAllToolData
  liftIO $ STM.atomically $ STM.writeTVar toolsVar freshTools
  pure freshTools

-- | Read all tools with metadata and runtime info
getAllToolData :: (IOE :> es) => Eff es (DMap Tool ToolData)
getAllToolData =
  DMap.fromList <$> forM allTools (\(Some tool) -> (tool :=>) <$> getToolData tool)
  where
    -- All tools to display (in desired order)
    allTools :: [Some Tool]
    allTools =
      [ Some Omnix
      , Some Git
      , Some Attic
      , Some Cachix
      , Some GitHub
      ]

-- | Get complete tool data (metadata + runtime info)
getToolData :: (IOE :> es) => Tool info -> Eff es (ToolData info)
getToolData tool = do
  info <- readToolInfo
  pure ToolData {name, description, url, binPaths, info}
  where
    name = case tool of
      Attic -> "Attic"
      GitHub -> "GitHub CLI"
      Omnix -> "Omnix"
      Git -> "Git"
      Cachix -> "Cachix"

    description = case tool of
      Attic -> "Self-hosted Nix binary cache server"
      GitHub -> "GitHub command line tool for various operations"
      Omnix -> "A tool for building all Nix flake outputs"
      Git -> "Distributed version control system"
      Cachix -> "Proprietary Nix binary cache hosting service"

    url = case tool of
      Attic -> "https://github.com/zhaofengli/attic"
      GitHub -> "https://cli.github.com"
      Omnix -> "https://github.com/juspay/omnix"
      Git -> "https://git-scm.com"
      Cachix -> "https://cachix.org"

    binPaths = case tool of
      Attic -> one $ toText Attic.atticBin
      GitHub -> toText GH.ghBin :| [toText GH.ghSignoffBin]
      Omnix -> one $ toText Omnix.omnixBin
      Git -> one $ toText Git.git
      Cachix -> one $ toText Cachix.cachixBin

    readToolInfo = case tool of
      Attic -> liftIO Attic.Config.readAtticConfig
      GitHub -> liftIO GH.checkAuthStatus
      Omnix -> pass
      Git -> pass
      Cachix -> pass
