module Vira.State.Core (
  -- * Types
  ViraState (..),
  Repo (..),
  Branch (..),
  Job (..),
  JobStatus (..),
  JobResult (..),

  -- * App initialization
  openViraState,
  closeViraState,
) where

import Data.Acid
import Data.Typeable (typeOf)
import System.FilePath ((</>))
import Vira.State.Acid
import Vira.State.Type

-- | Open vira database
openViraState :: FilePath -> IO (AcidState ViraState)
openViraState stateDir = do
  let initialState = ViraState mempty mempty mempty Nothing Nothing
  -- Manually construct the path that openLocalState would use: stateDir </> show (typeOf initialState)
  -- This is just for backwards compat.
  let acidStateDir = stateDir </> show (typeOf initialState)
  st <- openLocalStateFrom acidStateDir initialState
  update st MarkUnfinishedJobsAsStaleA
  pure st

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  closeAcidState st
