{- | Vira-specific entity filtering for the event system

This module provides Vira's domain-specific layer on top of the generic event bus.
It defines what "entities" exist in Vira and how to filter events by them.
-}
module Vira.App.Event.Entity (
  -- * Entity types
  EntityId (..),

  -- * Typeclass
  AffectedEntities (..),

  -- * Filtering helpers
  affectsEntity,
  affectsRepo,
  affectsJob,
  affectsAnyJob,
) where

import Data.Acid (EventResult, UpdateEvent)
import Data.Acid.Events (SomeUpdate (..))
import Data.Set qualified as Set
import Effectful.Git (RepoName)
import Vira.State.Type (JobId, ViraState)

-- * Entity types

-- | Identifiers for entities in Vira
data EntityId
  = RepoId RepoName
  | JobId JobId
  deriving stock (Eq, Ord, Show, Generic)

-- * Typeclass

{- | Typeclass for determining which entities an update affects

This is Vira-specific knowledge - different applications would have
different entity types and different ways of determining what's affected.
-}
class (UpdateEvent event) => AffectedEntities event where
  affectedEntities :: event -> EventResult event -> Set EntityId
  affectedEntities _ _ = Set.empty

-- * Filtering helpers

-- | Check if update affects a specific entity
affectsEntity :: EntityId -> SomeUpdate ViraState AffectedEntities -> Bool
affectsEntity entityId (SomeUpdate evt result _timestamp) =
  entityId `Set.member` affectedEntities evt result

-- | Check if update affects a specific repo
affectsRepo :: RepoName -> SomeUpdate ViraState AffectedEntities -> Bool
affectsRepo name = affectsEntity (RepoId name)

-- | Check if update affects a specific job
affectsJob :: JobId -> SomeUpdate ViraState AffectedEntities -> Bool
affectsJob jobId = affectsEntity (JobId jobId)

-- | Check if update affects any job
affectsAnyJob :: SomeUpdate ViraState AffectedEntities -> Bool
affectsAnyJob (SomeUpdate evt result _timestamp) =
  any isJobEntity (affectedEntities evt result)
  where
    isJobEntity (JobId _) = True
    isJobEntity _ = False
