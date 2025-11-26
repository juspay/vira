{- | Bitbucket configuration path

Follows XDG Base Directory specification for config file location.
-}
module Bitbucket.ConfigPath (
  getConfigPath,
) where

import System.Directory (XdgDirectory (..), getXdgDirectory)

{- | Get the Bitbucket config file path

Uses XDG Base Directory specification, typically:
- Linux/macOS: @$XDG_CONFIG_HOME/bb/config@ or @~/.config/bb/config@
- Windows: @%APPDATA%/bb/config@
-}
getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig "bb/config"
