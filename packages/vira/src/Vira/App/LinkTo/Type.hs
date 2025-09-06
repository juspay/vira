-- | Hints from across the apps to link to any route, but without needing a direct reference to all the routes (avoiding cylicic imports in Haskell)
module Vira.App.LinkTo.Type where

import Effectful.Git (BranchName)
import Vira.State.Type (JobId, RepoName)

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
  | Settings
  | SettingsUpdateCachix
  | SettingsDeleteCachix
  | SettingsUpdateAttic
  | SettingsDeleteAttic
  | Refresh

linkShortTitle :: LinkTo -> Text
linkShortTitle = \case
  Home -> "Vira"
  RepoListing -> "Repositories"
  Repo name -> toText . toString $ name
  RepoUpdate _ -> "Update" -- unused
  RepoDelete _ -> "Delete Repository"
  RepoAdd -> "Add Repository"
  Build _ _ -> "Build" -- unused
  RepoBranch _ branchName -> toText . toString $ branchName
  Job jobId -> "Job " <> show jobId
  JobLog jobId -> "Job Log " <> show jobId
  JobLogStream jobId -> "Job Log Stream " <> show jobId
  Kill _ -> "Kill" -- unused
  Settings -> "Settings"
  SettingsUpdateCachix -> "Cachix Settings" -- unused
  SettingsDeleteCachix -> "Delete Cachix Settings" -- unused
  SettingsUpdateAttic -> "Attic Settings" -- unused
  SettingsDeleteAttic -> "Delete Attic Settings" -- unused
  Refresh -> "Refresh"
