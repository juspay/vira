{-# LANGUAGE DuplicateRecordFields #-}

{- | Generic event bus for acid-state applications

This module provides a type-safe event bus that works with any acid-state application.
It has no domain-specific knowledge and could be extracted as a standalone library.

The event bus:
- Publishes Update events to subscribers via TChan
- Maintains a circular buffer of recent events for debugging
- Supports type-safe pattern matching on events via Typeable
-}
module Data.Acid.Events (
  -- * Core types
  SomeUpdate (..),
  EventBus (..),

  -- * Initialization
  newEventBus,

  -- * Event bus operations
  publishUpdate,
  subscribe,
  getRecentEvents,

  -- * Pattern matching
  matchUpdate,
) where

import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, isEmptyTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTChan, readTVarIO, writeTChan)
import Control.Monad (unless, void)
import Data.Acid (EventResult, EventState, UpdateEvent)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Text.Show qualified
import Unsafe.Coerce (unsafeCoerce)

-- * Core types

-- | Existential wrapper for any Update event with optional extra constraint
data SomeUpdate state constraint
  = forall event.
  ( UpdateEvent event
  , EventState event ~ state
  , Show event
  , Typeable event
  , constraint event
  ) =>
  SomeUpdate
  { update :: event
  , result :: EventResult event
  , timestamp :: UTCTime
  }

instance Text.Show.Show (SomeUpdate state constraint) where
  showsPrec d (SomeUpdate upd _result _time) = Text.Show.showsPrec d upd

-- | Event bus with broadcast channel and circular buffer log
data EventBus someUpdate = EventBus
  { channel :: TChan someUpdate
  , eventLog :: TVar (Seq someUpdate)
  , maxLogSize :: Int
  }
  deriving stock (Generic)

-- * Initialization

-- | Create a new event bus with default settings (1000 event buffer)
newEventBus :: IO (EventBus someUpdate)
newEventBus = newEventBusWithSize 1000

-- | Create a new event bus with custom buffer size
newEventBusWithSize :: Int -> IO (EventBus someUpdate)
newEventBusWithSize size = do
  channel <- newBroadcastTChanIO
  eventLog <- newTVarIO Seq.empty
  pure
    EventBus
      { channel
      , eventLog
      , maxLogSize = size
      }

-- * Event bus operations

-- | Publish an update event to all subscribers
publishUpdate ::
  EventBus someUpdate ->
  someUpdate ->
  IO ()
publishUpdate bus someUpdate = atomically $ do
  -- Publish to subscribers
  writeTChan (channel bus) someUpdate
  -- Append to circular buffer log
  modifyTVar' (eventLog bus) $ \eventLog' ->
    let newLog = eventLog' |> someUpdate
     in if Seq.length newLog > maxLogSize bus
          then Seq.drop 1 newLog
          else newLog

-- | Subscribe to events (returns duplicate channel starting from now)
subscribe ::
  EventBus someUpdate ->
  IO (TChan someUpdate)
subscribe bus = atomically $ do
  dup <- dupTChan (channel bus)
  -- Drain any pending events so subscriber starts fresh
  let drainLoop = do
        isEmpty <- isEmptyTChan dup
        unless isEmpty $ do
          void $ readTChan dup
          drainLoop
  drainLoop
  pure dup

-- | Get recent events from the log (for debug UI)
getRecentEvents ::
  EventBus someUpdate ->
  IO [someUpdate]
getRecentEvents bus = do
  eventLog' <- readTVarIO (eventLog bus)
  pure $ toList eventLog'

-- * Pattern matching

{- | Pattern match on specific update type

Note: Returns unsafe-coerced result since EventResult is a type family.
This is safe because if the update matches, the result type must match too.
-}
matchUpdate ::
  forall event state constraint.
  (UpdateEvent event, Typeable event, EventState event ~ state) =>
  SomeUpdate state constraint ->
  Maybe (event, EventResult event)
matchUpdate (SomeUpdate update result _timestamp) = do
  typedUpdate <- cast update
  -- Safe: if update type matches, result type must match (type family relation)
  pure (typedUpdate, unsafeCoerce result)
