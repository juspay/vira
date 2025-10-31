{-# LANGUAGE DuplicateRecordFields #-}

-- | Types for the event system
module Vira.App.Event.Type (
  -- * Core types
  SomeUpdate (..),
  TimestampedUpdate (..),
  EventBus (..),

  -- * Entity filtering (for SSE)
  AffectedEntities (..),

  -- * Initialization
  newEventBus,
) where

import Control.Concurrent.STM (TChan, newBroadcastTChanIO)
import Data.Acid (EventResult, EventState, UpdateEvent)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Effectful.Git (RepoName)
import Text.Show qualified
import Vira.State.Type (JobId, ViraState)

-- * Core types

-- | Existential wrapper for any Update event
data SomeUpdate
  = forall event.
  ( UpdateEvent event
  , EventState event ~ ViraState
  , Show event
  , Typeable event
  , AffectedEntities event
  ) =>
  SomeUpdate
  { update :: event
  , result :: EventResult event
  }

instance Text.Show.Show SomeUpdate where
  showsPrec d (SomeUpdate upd _result) = Text.Show.showsPrec d upd

-- | Timestamped update event
data TimestampedUpdate = TimestampedUpdate
  { timestamp :: UTCTime
  , update :: SomeUpdate
  }
  deriving stock (Show)

-- | Event bus with broadcast channel and circular buffer log
data EventBus = EventBus
  { channel :: TChan TimestampedUpdate
  , eventLog :: TVar (Seq TimestampedUpdate)
  , maxLogSize :: Int
  }
  deriving stock (Generic)

-- * Entity filtering

-- | Typeclass for determining which entities an update affects (for SSE filtering)
class (UpdateEvent event) => AffectedEntities event where
  affectedRepos :: event -> EventResult event -> [RepoName]
  affectedRepos _ _ = []

  affectedJobs :: event -> EventResult event -> [JobId]
  affectedJobs _ _ = []

-- * Initialization

-- | Create a new event bus
newEventBus :: IO EventBus
newEventBus = do
  channel <- newBroadcastTChanIO
  eventLog <- newTVarIO Seq.empty
  pure
    EventBus
      { channel
      , eventLog
      , maxLogSize = 1000
      }
