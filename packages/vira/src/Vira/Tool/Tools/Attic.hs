-- | Attic tool-specific logic
module Vira.Tool.Tools.Attic (
  getToolData,
) where

import Attic qualified
import Attic.Config qualified
import Effectful (Eff, IOE, (:>))
import TOML.Error qualified as TOML
import Vira.Tool.Type (ToolData (..))

-- | Get Attic tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData (Either TOML.TOMLError (Maybe Attic.Config.AtticConfig)))
getToolData = do
  info <- liftIO Attic.Config.readAtticConfig
  pure
    ToolData
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = one $ toText Attic.atticBin
      , info
      }
