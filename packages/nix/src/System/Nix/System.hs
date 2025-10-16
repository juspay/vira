-- | Nix system information
module System.Nix.System (
  nixSystem,
) where

import System.Info qualified as SysInfo

-- | Get the current Nix system string (e.g., "x86_64-linux", "aarch64-darwin")
nixSystem :: Text
nixSystem = toText SysInfo.arch <> "-" <> toText SysInfo.os
