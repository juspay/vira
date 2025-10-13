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
  startPeriodicArchival,

  -- * Version utilities
  viraDbVersion,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, handle)
import Data.Acid
import Data.Typeable (typeOf)
import System.FilePath ((</>))
import Vira.State.Acid qualified as Acid
import Vira.State.Reset (checkSchemaVersion, viraDbVersion, writeSchemaVersion)
import Vira.State.Type

-- | Open vira database
openViraState :: (HasCallStack) => FilePath -> Bool -> IO (AcidState ViraState)
openViraState stateDir autoResetState = do
  let initialState = ViraState mempty mempty mempty mempty
  -- Manually construct the path that openLocalState would use: stateDir </> show (typeOf initialState)
  -- This is just for backwards compat.
  let acidStateDir = stateDir </> show (typeOf initialState)

  -- Check and handle schema version
  checkSchemaVersion stateDir acidStateDir autoResetState

  -- Open the state
  putStrLn "Opening ViraState database..."
  st <- openLocalStateFrom acidStateDir initialState
  putStrLn $ "Opened ViraState; schema version = " <> show viraDbVersion

  -- Write current version after successful open
  writeSchemaVersion (stateDir </> "schema-version") viraDbVersion

  update st Acid.MarkUnfinishedJobsAsStaleA
  pure st

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  putStrLn "Creating checkpoint and archiving..."
  createCheckpoint st
  createArchive st
  closeAcidState st

-- | Start background thread for periodic checkpointing and archival
startPeriodicArchival :: AcidState ViraState -> IO ()
startPeriodicArchival st = void $ forkIO $ void $ infinitely $ do
  threadDelay (6 * 3600 * 1000000) -- 6 hours
  putStrLn "Creating checkpoint and archiving..."
  handle (\(e :: IOException) -> putStrLn $ "Archive failed: " <> show e) $ do
    createCheckpoint st
    createArchive st
