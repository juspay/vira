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
import Data.IxSet.Typed qualified as Ix
import Vira.State.Acid
import Vira.State.Type

-- | Open vira database
openViraState :: IO (AcidState ViraState)
openViraState = do
  -- TODO: Remove the hardcoding
  let repos = [Repo "vira" "https://github.com/juspay/vira.git" (RepoSettings ())]
  let appSettings = AppSettings (Ix.fromList repos) Nothing Nothing

  st <- openLocalState $ ViraState mempty mempty appSettings
  update st $ SetAppSettingsA appSettings
  -- update st $ SetAllReposA repos
  update st MarkUnfinishedJobsAsStaleA
  pure st

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  putStrLn "Closing ViraState"
  closeAcidState st
