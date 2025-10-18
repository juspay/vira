{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (
  ViraEnvironment (..),
  environmentFor,
  viraContext,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Process (Process)
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Type (ViraRuntimeState)
import Vira.CI.Context (ViraContext (..))
import Vira.State.Type (Branch (..), Repo)
import Vira.Tool.Core qualified as Tool
import Vira.Tool.Type.Tools (Tools)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: Repo
  , branch :: Branch
  , tools :: Tools
  -- ^ All tools with their runtime info (configs, auth status, etc.)
  , workspacePath :: FilePath
  -- ^ Workspace directory path
  }

-- | Construct the 'ViraEnvironment' for a given repository and branch.
environmentFor ::
  ( Process :> es
  , Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es ViraEnvironment
environmentFor repo branch workspacePath = do
  -- Refresh all tools with their latest runtime info
  tools <- Tool.refreshTools
  pure $ ViraEnvironment {..}

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  ViraContext
    { branch = env.branch.branchName
    , dirty = False
    }
