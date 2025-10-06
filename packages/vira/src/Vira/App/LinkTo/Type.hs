-- | Hints from across the apps to link to any route, but without needing a direct reference to all the routes (avoiding cylicic imports in Haskell)
module Vira.App.LinkTo.Type where

import Effectful.Git (BranchName, RepoName)
import Vira.State.Type (JobId)

{- | The part of the application the caller intends to link to

This is *roughy* isomorphic to the app routes, but only to the point they are needed for HTML links.
-}
data LinkTo
  = Home
  | RepoListing
  | Repo RepoName
  | RepoUpdate RepoName
  | RepoDelete RepoName
  | RepoAdd
  | Build RepoName BranchName
  | RepoBranch RepoName BranchName
  | Job JobId
  | JobLog JobId
  | JobLogStream JobId
  | Kill JobId
  | Tools
  | Refresh

linkShortTitle :: LinkTo -> Text
linkShortTitle = \case
  Home -> "Vira"
  RepoListing -> "Repositories"
  Repo name -> toStringText name
  RepoUpdate _ -> "Update" -- unused
  RepoDelete _ -> "Delete Repository"
  RepoAdd -> "Add Repository"
  Build _ _ -> "Build" -- unused
  RepoBranch _ branchName -> toStringText branchName
  Job jobId -> "Job " <> show jobId
  JobLog jobId -> "Job Log " <> show jobId
  JobLogStream jobId -> "Job Log Stream " <> show jobId
  Kill _ -> "Kill" -- unused
  Tools -> "Tools"
  Refresh -> "Refresh"

linkTitle :: LinkTo -> Text
linkTitle = \case
  RepoBranch r b -> toStringText r <> " → " <> toStringText b
  x -> linkShortTitle x

toStringText :: (ToString a) => a -> Text
toStringText = toText . toString
