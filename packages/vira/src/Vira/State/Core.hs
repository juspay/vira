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
import Vira.State.Acid
import Vira.State.Type

-- | Open vira database
openViraState :: FilePath -> IO (AcidState ViraState)
openViraState stateDir = do
  st <- openLocalStateFrom stateDir $ ViraState mempty mempty mempty Nothing Nothing
  update st MarkUnfinishedJobsAsStaleA
  pure st

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  putStrLn "Closing ViraState"
  closeAcidState st
