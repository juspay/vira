module Vira.State.Core (
  -- * Types
  ViraState (..),
  Repo (..),
  Branch (..),
  Job (..),
  JobStatus (..),
  JobResult (..),
  jobIsActive,
  jobEndTime,

  -- * App initialization
  openViraState,
  closeViraState,

  -- * Version utilities
  versionToInt,
  viraDbVersion,
) where

import Data.Acid
import Data.SafeCopy (SafeCopy (version), Version)
import Data.Typeable (typeOf)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Vira.State.Acid (ViraState (..))
import Vira.State.Acid qualified as Acid
import Vira.State.Type

-- Get the version number from a SafeCopy Version type
-- Since Version has Num instance, we use negate trick: negate (negate v) extracts the Int
versionToInt :: forall a. Version a -> Int
versionToInt v
  | v == 0 = 0
  | otherwise = versionToInt (v - 1) + 1

-- | Get the current ViraState database schema version
viraDbVersion :: Int
viraDbVersion = versionToInt (version @ViraState)

-- | Open vira database
openViraState :: (HasCallStack) => FilePath -> Bool -> IO (AcidState ViraState)
openViraState stateDir autoResetDb = do
  let initialState = ViraState mempty mempty mempty mempty
  -- Manually construct the path that openLocalState would use: stateDir </> show (typeOf initialState)
  -- This is just for backwards compat.
  let acidStateDir = stateDir </> show (typeOf initialState)
      workspaceDir = stateDir </> "workspace"
      versionFile = stateDir </> "schema-version"
      -- Get SafeCopy version dynamically from ViraState type
      currentVersion = viraDbVersion

  -- Check if state directory exists
  stateExists <- doesDirectoryExist acidStateDir

  -- Check schema version compatibility
  if stateExists
    then do
      mStoredVersion <- readSchemaVersion versionFile
      case mStoredVersion of
        Nothing -> do
          -- No version file - either legacy state or corrupted
          if autoResetDb
            then resetState acidStateDir workspaceDir versionFile
            else handleMissingVersion stateDir
        Just storedVersion -> do
          if storedVersion /= currentVersion
            then do
              -- Version mismatch detected
              if autoResetDb
                then do
                  putStrLn $ "WARNING: Schema version mismatch detected."
                  putStrLn $ "  Stored version: " <> show storedVersion
                  putStrLn $ "  Current version: " <> show currentVersion
                  resetState acidStateDir workspaceDir versionFile
                else handleVersionMismatch currentVersion stateDir storedVersion
            else pure () -- Version matches, continue normally
    else pure () -- Fresh state, no version check needed

  -- Open the state
  st <- openLocalStateFrom acidStateDir initialState

  -- Write current version after successful open
  writeSchemaVersion versionFile currentVersion

  update st Acid.MarkUnfinishedJobsAsStaleA
  pure st
  where
    resetState acidStateDir workspaceDir versionFile = do
      putStrLn "Auto-reset is enabled - cleaning state directories and starting fresh."
      putStrLn ""
      putStrLn ("Removing: " <> acidStateDir)
      whenM (doesDirectoryExist acidStateDir) $
        removeDirectoryRecursive acidStateDir
      putStrLn ("Removing: " <> workspaceDir)
      whenM (doesDirectoryExist workspaceDir) $
        removeDirectoryRecursive workspaceDir
      -- Remove version file to start fresh
      whenM (doesFileExist versionFile) $
        removeFile versionFile
      putStrLn ""

    handleMissingVersion dir = do
      putStrLn "ERROR: Found existing state but no schema version file."
      putStrLn "This may indicate a legacy state or corruption."
      putStrLn "Please remove the vira data directory and restart:"
      putStrLn ("  rm -rf " <> dir)
      putStrLn "Or use the --auto-reset-db flag to automatically reset on schema changes."
      exitFailure

    handleVersionMismatch currentVer dir storedVersion = do
      putStrLn $ "ERROR: Schema version mismatch."
      putStrLn $ "  Stored version: " <> show storedVersion
      putStrLn $ "  Current version: " <> show currentVer
      putStrLn ""
      putStrLn "Your state is incompatible with this version of Vira."
      putStrLn "Please remove the vira data directory and restart:"
      putStrLn ("  rm -rf " <> dir)
      putStrLn "Or use the --auto-reset-db flag to automatically reset on schema changes."
      exitFailure

    readSchemaVersion :: FilePath -> IO (Maybe Int)
    readSchemaVersion path = do
      exists <- doesFileExist path
      if exists
        then do
          content <- readFileBS path
          pure $ readMaybe (decodeUtf8 content)
        else pure Nothing

    writeSchemaVersion :: FilePath -> Int -> IO ()
    writeSchemaVersion path ver = do
      writeFileBS path (encodeUtf8 @Text $ show ver)

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  closeAcidState st
