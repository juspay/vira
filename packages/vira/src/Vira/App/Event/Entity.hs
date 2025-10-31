{- | Vira-specific entity filtering for the event system

This module provides Vira's domain-specific layer on top of the generic event bus.
It defines what "entities" exist in Vira and how to filter events by them.
-}
module Vira.App.Event.Entity (
  -- * Entity types
  EntityId (..),

  -- * Typeclass
  AffectedEntities (..),

  -- * Vira-specific SomeUpdate wrapper
  ViraSomeUpdate (..),

  -- * Filtering helpers
  affectsEntity,
  affectsRepo,
  affectsJob,
  affectsAnyJob,

  -- * Pattern matching
  matchUpdate,
) where

import Data.Acid (EventResult, EventState, UpdateEvent)
import Data.Set qualified as Set
import Data.Typeable (cast)
import Effectful.Git (RepoName)
import Text.Show qualified
import Unsafe.Coerce (unsafeCoerce)
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

-- * Vira-specific SomeUpdate wrapper

{- | Vira-specific SomeUpdate with AffectedEntities constraint

This adds the AffectedEntities constraint on top of the generic Core.SomeUpdate,
allowing us to filter events by entity.
-}
data ViraSomeUpdate
  = forall event.
  ( UpdateEvent event
  , EventState event ~ ViraState
  , Show event
  , Typeable event
  , AffectedEntities event
  ) =>
  ViraSomeUpdate
  { update :: event
  , result :: EventResult event
  }

instance Text.Show.Show ViraSomeUpdate where
  showsPrec d (ViraSomeUpdate upd _result) = Text.Show.showsPrec d upd

-- * Filtering helpers

-- | Check if update affects a specific entity
affectsEntity :: EntityId -> ViraSomeUpdate -> Bool
affectsEntity entityId (ViraSomeUpdate update result) =
  entityId `Set.member` affectedEntities update result

-- | Check if update affects a specific repo
affectsRepo :: RepoName -> ViraSomeUpdate -> Bool
affectsRepo name = affectsEntity (RepoId name)

-- | Check if update affects a specific job
affectsJob :: JobId -> ViraSomeUpdate -> Bool
affectsJob jobId = affectsEntity (JobId jobId)

-- | Check if update affects any job
affectsAnyJob :: ViraSomeUpdate -> Bool
affectsAnyJob (ViraSomeUpdate update result) =
  any isJobEntity (affectedEntities update result)
  where
    isJobEntity (JobId _) = True
    isJobEntity _ = False

-- * Pattern matching

{- | Pattern match on specific update type (Vira-specific version)

Note: Returns unsafe-coerced result since EventResult is a type family.
This is safe because if the update matches, the result type must match too.
-}
matchUpdate ::
  forall event.
  (UpdateEvent event, Typeable event, EventState event ~ ViraState) =>
  ViraSomeUpdate ->
  Maybe (event, EventResult event)
matchUpdate (ViraSomeUpdate update result) = do
  typedUpdate <- cast update
  -- Safe: if update type matches, result type must match (type family relation)
  pure (typedUpdate, unsafeCoerce result)
