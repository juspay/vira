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
  st <- openLocalState $ ViraState sampleRepos mempty mempty
  update st MarkRunningJobsAsStaleA
  pure st
  where
    sampleRepos =
      Ix.fromList
        [ Repo "haskell-template" "https://github.com/srid/haskell-template.git"
        ]

{- | Close vira database

It is imperative to call this before shutting down the application, else the state can remain locked.
-}
closeViraState :: AcidState ViraState -> IO ()
closeViraState st = do
  putStrLn "Closing ViraState"
  closeAcidState st
