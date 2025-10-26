-- | Nix system information
module System.Nix.System (
  System (..),
  nixSystem,
) where

import Data.Aeson (FromJSON)
import System.Info qualified as SysInfo

-- | Nix system identifier (e.g., "x86_64-linux", "aarch64-darwin")
newtype System = System {unSystem :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToString, IsString, FromJSON)

-- | Get the current Nix system
nixSystem :: System
nixSystem = System $ toText SysInfo.arch <> "-" <> toText SysInfo.os
