{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | JSON types for import/export functionality
module Vira.State.JSON (
  ViraExportData (..),
  ExportRepo (..),
  getExportData,
  importViraState,
) where

import Data.Acid (AcidState, query, update)
import Data.Aeson (FromJSON (..), ToJSON (..), decode)
import Data.ByteString.Lazy qualified as LBS
import Vira.State.Acid (AddNewRepoA (AddNewRepoA), GetAllReposA (GetAllReposA), GetAtticSettingsA (GetAtticSettingsA), GetCachixSettingsA (GetCachixSettingsA), GetRepoByNameA (GetRepoByNameA), SetAtticSettingsA (SetAtticSettingsA), SetCachixSettingsA (SetCachixSettingsA), ViraState)
import Vira.State.Type (AtticSettings, CachixSettings, Repo (..), RepoName, RepoSettings (..))

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

-- | Import Vira state from JSON data
importViraState :: AcidState ViraState -> LBS.ByteString -> IO (Either String ())
importViraState acid jsonData =
  case decode jsonData :: Maybe ViraExportData of
    Nothing -> pure $ Left "Invalid JSON format"
    Just (ViraExportData repos cachix attic) -> do
      -- Process repositories individually
      result <- importRepositories acid repos
      case result of
        Left err -> pure $ Left err
        Right () -> do
          -- Update settings only if repository import succeeded
          update acid (SetCachixSettingsA cachix)
          update acid (SetAtticSettingsA attic)
          pure $ Right ()

-- | Import repositories, checking for conflicts
importRepositories :: AcidState ViraState -> [ExportRepo] -> IO (Either String ())
importRepositories acid repos = do
  results <- mapM (importSingleRepo acid) repos
  case lefts results of
    [] -> pure $ Right ()
    errs -> pure $ Left $ toString ("Repository import failed:\n" <> unlines (map toText errs))

-- | Import a single repository, checking for conflicts
importSingleRepo :: AcidState ViraState -> ExportRepo -> IO (Either String ())
importSingleRepo acid (ExportRepo name url) = do
  existingRepo <- query acid (GetRepoByNameA name)
  case existingRepo of
    Nothing -> do
      -- Repository doesn't exist, add it
      let newRepo = Repo name url (RepoSettings Nothing)
      update acid (AddNewRepoA newRepo)
      pure $ Right ()
    Just (Repo _ existingUrl _) -> do
      -- Repository exists, check if URL matches
      if existingUrl == url
        then pure $ Right () -- Same URL, no conflict
        else
          pure $
            Left $
              "Repository '"
                <> show name
                <> "' already exists with different URL:\n"
                <> "  Existing: "
                <> show existingUrl
                <> "\n"
                <> "  Import:   "
                <> show url
