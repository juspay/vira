-- | Database reset and schema version management
module Vira.State.Reset (
  checkSchemaVersion,
  viraDbVersion,
  writeSchemaVersion,

  -- * Used in tests
  versionToInt,
) where

import Data.SafeCopy (SafeCopy (version), Version)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Unsafe.Coerce (unsafeCoerce)
import Vira.State.Acid (ViraState)

-- * Public API

-- | Get the current ViraState database schema version
viraDbVersion :: Int
viraDbVersion = versionToInt (version @ViraState)

-- | Check schema version compatibility and handle mismatches
checkSchemaVersion :: FilePath -> FilePath -> Bool -> IO ()
checkSchemaVersion stateDir acidStateDir autoResetState = do
  let workspaceDir = stateDir </> "workspace"
      versionFile = stateDir </> "schema-version"

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
      putStrLn "Auto-reset is enabled - cleaning state (ViraState and job workspaces)."
      putStrLn ""

      -- Remove ViraState directory
      whenM (doesDirectoryExist acidDir) $ do
        putStrLn ("Removing: " <> acidDir)
        removeDirectoryRecursive acidDir

      -- Remove job directories under workspace/*/jobs
      whenM (doesDirectoryExist workspace) $ do
        repos <- listDirectory workspace
        forM_ repos $ \repo -> do
          let jobsDir = workspace </> repo </> "jobs"
          whenM (doesDirectoryExist jobsDir) $ do
            putStrLn ("Removing: " <> jobsDir)
            removeDirectoryRecursive jobsDir

      -- Remove version file to start fresh
      whenM (doesFileExist vFile) $
        removeFile vFile

      putStrLn ""

    handleMissingVersion dir = do
      putStrLn "ERROR: Found existing state but no schema version file."
      putStrLn "This may indicate a legacy state or corruption."
      showManualResetInstructions dir

    handleVersionMismatch currentVer dir storedVersion = do
      putStrLn "ERROR: Schema version mismatch."
      putStrLn $ "  Stored version: " <> show storedVersion
      putStrLn $ "  Current version: " <> show currentVer
      putStrLn ""
      putStrLn "Your state is incompatible with this version of Vira."
      showManualResetInstructions dir

    showManualResetInstructions dir = do
      putStrLn "Please remove ViraState and job workspaces manually and restart:"
      putStrLn ("  rm -rf " <> dir </> "ViraState")
      putStrLn ("  rm -rf " <> dir </> "workspace" </> "*" </> "jobs")
      putStrLn "Or use the --auto-reset-state flag to automatically reset on schema changes."
      exitFailure

-- * Internal helpers

{- | Get the version number from a SafeCopy Version type in O(1)
Version is a newtype wrapper around Int32. Since the constructor isn't exported,
we use unsafeCoerce to unwrap it directly. This is safe because Version is
literally `newtype Version a = Version Int32`.
-}
versionToInt :: forall a. Version a -> Int
versionToInt v = fromIntegral (unsafeCoerce v :: Int32)

-- | Read the stored schema version from disk
readSchemaVersion :: FilePath -> IO (Maybe Int)
readSchemaVersion path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFileBS path
      pure $ readMaybe (decodeUtf8 content)
    else pure Nothing

-- | Write the current schema version to disk
writeSchemaVersion :: FilePath -> Int -> IO ()
writeSchemaVersion path ver = do
  writeFileBS path (encodeUtf8 @Text $ show ver)
