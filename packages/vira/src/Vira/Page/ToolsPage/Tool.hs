{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tool definitions and data operations
module Vira.Page.ToolsPage.Tool (
  Tool (..),
  ToolData (..),
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

-- | GADT for tool keys with their info types inlined
data Tool info where
  Attic :: Tool (Either TOMLError (Maybe AtticConfig))
  GitHub :: Tool AuthStatus
  Omnix :: Tool ()
  Git :: Tool ()
  Cachix :: Tool ()

$(deriveGEq ''Tool)
$(deriveGCompare ''Tool)

-- | Tool data combining metadata and runtime info
data ToolData info = ToolData
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  , info :: info
  }

-- | All tools to display (in desired order)
toolsOrder :: [DSum Tool (Const ())]
toolsOrder =
  [ Omnix :=> Const ()
  , Git :=> Const ()
  , Attic :=> Const ()
  , Cachix :=> Const ()
  , GitHub :=> Const ()
  ]

-- | Read all tools with metadata and runtime info (preserves order)
readAllTools :: IO [DSum Tool ToolData]
readAllTools = forM toolsOrder $ \(tool :=> _) -> do
  toolData <- getToolData tool
  pure $ tool :=> toolData

-- | Get complete tool data (metadata + runtime info)
getToolData :: Tool info -> IO (ToolData info)
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
      Attic -> Attic.Config.readAtticConfig
      GitHub -> GH.checkAuthStatus
      Omnix -> pass
      Git -> pass
      Cachix -> pass
