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
  viraDbVersion,
) where

import Data.Acid
import Data.Typeable (typeOf)
import System.FilePath ((</>))
import Vira.State.Acid (ViraState (..))
import Vira.State.Acid qualified as Acid
import Vira.State.Reset (checkSchemaVersion, viraDbVersion)
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
  st <- openLocalStateFrom acidStateDir initialState

  -- Write current version after successful open
  writeSchemaVersion (stateDir </> "schema-version") viraDbVersion

  update st Acid.MarkUnfinishedJobsAsStaleA
  pure st

-- | Write the current schema version to disk
writeSchemaVersion :: FilePath -> Int -> IO ()
writeSchemaVersion path ver = do
  writeFileBS path (encodeUtf8 @Text $ show ver)

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  closeAcidState st
