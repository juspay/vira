{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tool definitions and data operations
module Vira.Page.ToolsPage.Tool (
  Tool (..),
  ToolMeta (..),
  toolMeta,
  readAllTools,
) where

import Attic qualified
import Attic.Config (AtticConfig (..))
import Attic.Config qualified
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Effectful.Git qualified as Git
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import TOML (TOMLError)
import Vira.Lib.Cachix qualified as Cachix
import Vira.Lib.Omnix qualified as Omnix

-- | Tool metadata
data ToolMeta = ToolMeta
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  }

-- | GADT for tool keys with their info types inlined
data Tool info where
  Attic :: Tool (Either TOMLError (Maybe AtticConfig))
  GitHub :: Tool AuthStatus
  Omnix :: Tool ()
  Git :: Tool ()
  Cachix :: Tool ()

$(deriveGEq ''Tool)
$(deriveGCompare ''Tool)

-- | Get metadata for a tool
toolMeta :: Tool info -> ToolMeta
toolMeta = \case
  Attic ->
    ToolMeta
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = toText Attic.atticBin :| []
      }
  GitHub ->
    ToolMeta
      { name = "GitHub CLI"
      , description = "GitHub command line tool for various operations"
      , url = "https://cli.github.com"
      , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
      }
  Omnix ->
    ToolMeta
      { name = "Omnix"
      , description = "A tool for building all Nix flake outputs"
      , url = "https://github.com/juspay/omnix"
      , binPaths = toText Omnix.omnixBin :| []
      }
  Git ->
    ToolMeta
      { name = "Git"
      , description = "Distributed version control system"
      , url = "https://git-scm.com"
      , binPaths = toText Git.git :| []
      }
  Cachix ->
    ToolMeta
      { name = "Cachix"
      , description = "Proprietary Nix binary cache hosting service"
      , url = "https://cachix.org"
      , binPaths = toText Cachix.cachixBin :| []
      }

-- | Read runtime information for a tool
readInfo :: Tool info -> IO info
readInfo = \case
  Attic -> Attic.Config.readAtticConfig
  GitHub -> GH.checkAuthStatus
  Omnix -> pass
  Git -> pass
  Cachix -> pass

-- | All tools to display (in desired order)
allTools :: [DSum Tool (Const ())]
allTools =
  [ Omnix :=> Const ()
  , Git :=> Const ()
  , Attic :=> Const ()
  , Cachix :=> Const ()
  , GitHub :=> Const ()
  ]

-- | Read runtime information for all tools (preserves order)
readAllTools :: IO [DSum Tool Identity]
readAllTools = forM allTools $ \(tool :=> _) -> do
  info <- readInfo tool
  pure (tool :=> Identity info)
