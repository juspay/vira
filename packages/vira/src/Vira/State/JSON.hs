{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | JSON types for import/export functionality
module Vira.State.JSON (
  ViraExportData (..),
  ExportRepo (..),
  getExportData,
) where

import Data.Acid (AcidState, query)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Vira.State.Acid (GetAllReposA (GetAllReposA), GetAtticSettingsA (GetAtticSettingsA), GetCachixSettingsA (GetCachixSettingsA), ViraState)
import Vira.State.Type (AtticSettings, CachixSettings, Repo (..), RepoName)

-- | Subset of ViraState that can be exported/imported
data ViraExportData = ViraExportData
  { repositories :: [ExportRepo]
  -- ^ List of repositories with name and clone URL
  , cachixSettings :: Maybe CachixSettings
  -- ^ Global Cachix settings
  , atticSettings :: Maybe AtticSettings
  -- ^ Global Attic settings
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Simplified repository data for export/import
data ExportRepo = ExportRepo
  { name :: RepoName
  -- ^ Repository name
  , cloneUrl :: Text
  -- ^ Git clone URL
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Get export data by querying acid state
getExportData :: AcidState ViraState -> IO ViraExportData
getExportData acid = do
  repos <- query acid GetAllReposA
  cachix <- query acid GetCachixSettingsA
  attic <- query acid GetAtticSettingsA
  pure $
    ViraExportData
      { repositories = map (\r -> ExportRepo r.name r.cloneUrl) repos
      , cachixSettings = cachix
      , atticSettings = attic
      }
