{-# LANGUAGE DeriveAnyClass #-}

{- |
Typeclass for determining which entities an update affects (used for SSE filtering).

This module is intentionally minimal to avoid cyclic imports.
-}
module Vira.Web.Stream.AffectedEntities (
  AffectedEntities (..),
  EntityId (..),
) where

import Data.Acid (EventResult, UpdateEvent)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set qualified as Set
import Effectful.Git (RepoName)
import Vira.State.Type (JobId)

-- | Identifiers for entities in Vira
data EntityId
  = RepoId RepoName
  | JobId JobId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Typeclass for determining which entities an update affects (used for SSE filtering)
class (UpdateEvent event) => AffectedEntities event where
  affectedEntities :: event -> EventResult event -> Set EntityId
  affectedEntities _ _ = Set.empty
