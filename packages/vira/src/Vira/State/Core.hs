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
) where

import Control.Exception (ErrorCall (..), catch)
import Data.Acid
import Data.Typeable (typeOf)
import System.FilePath ((</>))
import Vira.State.Acid
import Vira.State.Type

-- | Open vira database
openViraState :: (HasCallStack) => FilePath -> IO (AcidState ViraState)
openViraState stateDir = do
  let initialState = ViraState mempty mempty mempty mempty Nothing Nothing
  -- Manually construct the path that openLocalState would use: stateDir </> show (typeOf initialState)
  -- This is just for backwards compat.
  let acidStateDir = stateDir </> show (typeOf initialState)
  st <- openLocalStateFrom acidStateDir initialState `catch` handleStateError
  update st MarkUnfinishedJobsAsStaleA
  pure st
  where
    handleStateError (ErrorCall msg) = do
      putStrLn "ERROR: Failed to open acid-state database. This usually indicates incompatible state format."
      putStrLn "Please remove the vira data directory and restart:"
      putStrLn ("  rm -rf " <> stateDir)
      putStrLn "Your data will be lost, but this is necessary to continue."
      putStrLn ""
      putStrLn "Original error:"
      putStrLn msg
      exitFailure

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  closeAcidState st
