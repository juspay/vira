-- | Database reset and schema version management
module Vira.State.Reset (
  checkSchemaVersion,
  viraDbVersion,
) where

import Data.SafeCopy (SafeCopy (version), Version)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import Vira.State.Acid (ViraState)

-- * Public API

-- | Get the current ViraState database schema version
viraDbVersion :: Int
viraDbVersion = versionToInt (version @ViraState)

-- | Check schema version compatibility and handle mismatches
checkSchemaVersion :: FilePath -> FilePath -> FilePath -> FilePath -> Bool -> IO ()
checkSchemaVersion stateDir acidStateDir workspaceDir versionFile autoResetState = do
  stateExists <- doesDirectoryExist acidStateDir
  when stateExists $ do
    mStoredVersion <- readSchemaVersion versionFile
    case mStoredVersion of
      Nothing -> do
        -- No version file - either legacy state or corrupted
        if autoResetState
          then resetState acidStateDir workspaceDir versionFile
          else handleMissingVersion stateDir
      Just storedVersion -> do
        when (storedVersion /= viraDbVersion) $ do
          -- Version mismatch detected
          if autoResetState
            then do
              putStrLn "WARNING: Schema version mismatch detected."
              putStrLn $ "  Stored version: " <> show storedVersion
              putStrLn $ "  Current version: " <> show viraDbVersion
              resetState acidStateDir workspaceDir versionFile
            else handleVersionMismatch viraDbVersion stateDir storedVersion
  where
    resetState acidDir workspace vFile = do
      putStrLn "Auto-reset is enabled - cleaning state directories and starting fresh."
      putStrLn ""
      putStrLn ("Removing: " <> acidDir)
      whenM (doesDirectoryExist acidDir) $
        removeDirectoryRecursive acidDir
      putStrLn ("Removing: " <> workspace)
      whenM (doesDirectoryExist workspace) $
        removeDirectoryRecursive workspace
      -- Remove version file to start fresh
      whenM (doesFileExist vFile) $
        removeFile vFile
      putStrLn ""

    handleMissingVersion dir = do
      putStrLn "ERROR: Found existing state but no schema version file."
      putStrLn "This may indicate a legacy state or corruption."
      putStrLn "Please remove the vira data directory and restart:"
      putStrLn ("  rm -rf " <> dir)
      putStrLn "Or use the --auto-reset-state flag to automatically reset on schema changes."
      exitFailure

    handleVersionMismatch currentVer dir storedVersion = do
      putStrLn "ERROR: Schema version mismatch."
      putStrLn $ "  Stored version: " <> show storedVersion
      putStrLn $ "  Current version: " <> show currentVer
      putStrLn ""
      putStrLn "Your state is incompatible with this version of Vira."
      putStrLn "Please remove the vira data directory and restart:"
      putStrLn ("  rm -rf " <> dir)
      putStrLn "Or use the --auto-reset-state flag to automatically reset on schema changes."
      exitFailure

-- * Internal helpers

{- | Get the version number from a SafeCopy Version type
Since Version has Num instance, we use recursive subtraction to extract the Int
-}
versionToInt :: forall a. Version a -> Int
versionToInt v
  | v == 0 = 0
  | otherwise = versionToInt (v - 1) + 1

-- | Read the stored schema version from disk
readSchemaVersion :: FilePath -> IO (Maybe Int)
readSchemaVersion path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFileBS path
      pure $ readMaybe (decodeUtf8 content)
    else pure Nothing
