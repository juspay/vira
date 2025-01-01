-- | Hints from across the apps to link to any route, but without needing a direct reference to all the routes (avoiding cylicic imports in Haskell)
module Vira.App.LinkTo where

import Vira.Lib.Git (BranchName)
import Vira.State.Type (RepoName)

{- | The part of the application the caller intends to link to

This is *roughy* isomorphic to the app routes, but only to the point they are needed for HTML links.
-}
data LinkTo
  = Home
  | RepoListing
  | Repo RepoName
  | RepoUpdate RepoName
  | RepoBranchJobs RepoName BranchName
  | Build RepoName BranchName
  | About

linkShortTitle :: LinkTo -> Text
linkShortTitle = \case
  Home -> "Vira"
  RepoListing -> "Repos"
  Repo name -> toText . toString $ name
  RepoUpdate _ -> "Update" -- unused
  RepoBranchJobs _ _ -> "Jobs" -- unused
  Build _ _ -> "Build" -- unused
  About -> "About"
