-- | Hints from across the apps to link to any route, but without needing a direct reference to all the routes (avoiding cylicic imports in Haskell)
module Vira.App.LinkTo.Type where

import Vira.Lib.Git (BranchName)
import Vira.State.Type (JobId, RepoName)

{- | The part of the application the caller intends to link to

This is *roughy* isomorphic to the app routes, but only to the point they are needed for HTML links.
-}
data LinkTo
  = Home
  | RepoListing
  | Repo RepoName
  | RepoUpdate RepoName
  | Build RepoName BranchName
  | Job JobId
  | JobLog JobId
  | StatusGet
  | About

linkShortTitle :: LinkTo -> Text
linkShortTitle = \case
  Home -> "Vira"
  RepoListing -> "Repos"
  Repo name -> toText . toString $ name
  RepoUpdate _ -> "Update" -- unused
  Build _ _ -> "Build" -- unused
  Job jobId -> "Job " <> show jobId
  JobLog jobId -> "Job Log " <> show jobId
  StatusGet -> "Status"
  About -> "About"
