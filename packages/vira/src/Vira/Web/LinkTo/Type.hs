-- | Hints from across the apps to link to any route, but without needing a direct reference to all the routes (avoiding cylicic imports in Haskell)
module Vira.Web.LinkTo.Type where

import Effectful.Git (BranchName, RepoName)
import Vira.State.Type (BranchFilter, JobId)

{- | The part of the application the caller intends to link to

This is roughly isomorphic to the app routes, but only to the point they are needed for HTML links.
Avoids cyclic imports by providing a type-level hint instead of direct route references.

Used with 'Vira.Web.Lucid.getLink' to generate 'Servant.Links.Link' values.
-}
data LinkTo
  = Home BranchFilter
  | RepoListing
  | Repo RepoName
  | RepoUpdate RepoName
  | RepoDelete RepoName
  | RepoAdd
  | RepoBranchFilter RepoName
  | Build RepoName BranchName
  | RepoBranch RepoName BranchName
  | Job JobId
  | JobLog JobId
  | JobLogStream JobId
  | Kill JobId
  | Environment
  | Cache
  | Events
  | Refresh (Maybe Text) -- Query parameter for event patterns

linkShortTitle :: LinkTo -> Text
linkShortTitle = \case
  Home _ -> "Vira"
  RepoListing -> "Repositories"
  Repo name -> toStringText name
  RepoUpdate _ -> "Update" -- unused
  RepoDelete _ -> "Delete Repository"
  RepoAdd -> "Add Repository"
  RepoBranchFilter _ -> "Filter Branches" -- unused
  Build _ _ -> "Build" -- unused
  RepoBranch _ branchName -> toStringText branchName
  Job jobId -> "Job " <> show jobId
  JobLog jobId -> "Job Log " <> show jobId
  JobLogStream jobId -> "Job Log Stream " <> show jobId
  Kill _ -> "Kill" -- unused
  Environment -> "Environment"
  Cache -> "Binary Cache"
  Events -> "Events"
  Refresh _ -> "Refresh"

linkTitle :: LinkTo -> Text
linkTitle = \case
  RepoBranch r b -> toStringText r <> " → " <> toStringText b
  x -> linkShortTitle x

toStringText :: (ToString a) => a -> Text
toStringText = toText . toString
