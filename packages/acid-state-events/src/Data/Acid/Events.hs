{-# LANGUAGE DuplicateRecordFields #-}

{- | Event bus for acid-state applications

This module provides a type-safe event bus that works with any acid-state application.

The event bus:
- Publishes 'SomeUpdate' events to subscribers via 'TChan'
- Maintains a circular buffer of recent events for debugging
- Supports type-safe pattern matching on events via 'Typeable'
-}
module Data.Acid.Events (
  -- * Core types
  SomeUpdate (..),
  EventBus (..),

  -- * Initialization
  newEventBus,

  -- * acid-state wrappers
  update,

  -- * Event bus operations
  subscribe,
  awaitBatched,
  getRecentEvents,

  -- * Pattern matching
  matchUpdate,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, isEmptyTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTChan, readTVarIO, tryReadTChan, writeTChan)
import Control.Monad (unless, void)
import Data.Acid (AcidState, EventResult, EventState, UpdateEvent)
import Data.Acid qualified as Acid
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Text.Show qualified
import Unsafe.Coerce (unsafeCoerce)

-- * Core types

-- | Existential wrapper for any Update event
data SomeUpdate state
  = forall event.
  ( UpdateEvent event
  , EventState event ~ state
  , Show event
  , Typeable event
  ) =>
  SomeUpdate
  { event :: event
  , result :: EventResult event
  , timestamp :: UTCTime
  }

instance Text.Show.Show (SomeUpdate state) where
  showsPrec d (SomeUpdate evt _result _time) = Text.Show.showsPrec d evt

-- | Event bus with broadcast channel and circular buffer log of 'SomeUpdate' events
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

-- | Publish an update event to all subscribers (internal)
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

{- | Execute an acid-state update and automatically publish to event bus

This is the main entry point for executing updates - it handles both
the acid-state transaction and event publishing in one operation.
-}
update ::
  ( UpdateEvent event
  , EventState event ~ state
  , Show event
  , Typeable event
  ) =>
  AcidState state ->
  EventBus (SomeUpdate state) ->
  event ->
  IO (EventResult event)
update acid bus event = do
  result <- Acid.update acid event
  timestamp <- getCurrentTime
  publishUpdate bus (SomeUpdate event result timestamp)
  pure result

-- | Subscribe to events (returns duplicate 'TChan' starting from now)
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

{- | Wait for and collect matching events with debouncing

Useful for batching rapid event sequences (e.g., multiple file saves)
into a single processing cycle. Blocks until first matching event,
then collects additional matches during debounce window.

Example: Wait for repo updates, batch them over 2.5s window
@
  batch <- awaitBatched chan isRepoUpdate 2_500_000
  processBatch batch
@
-}
awaitBatched ::
  TChan someUpdate ->
  -- | Predicate to filter events
  (someUpdate -> Bool) ->
  -- | Debounce window in microseconds
  Int ->
  IO (NonEmpty someUpdate)
awaitBatched chan predicate debounceUs = do
  -- Block until first matching event
  firstUpdate <- atomically waitForMatch
  -- Debounce: wait for more events to batch together
  threadDelay debounceUs
  -- Collect any additional matching events
  moreUpdates <- drainMatching []
  pure $ firstUpdate :| moreUpdates
  where
    -- Wait for an update that matches predicate
    waitForMatch = do
      evt <- readTChan chan
      if predicate evt
        then pure evt
        else waitForMatch -- Retry in STM - keep reading until match

    -- Drain additional matching updates (non-blocking)
    drainMatching acc = do
      mUpdate <- atomically $ tryReadTChan chan
      case mUpdate of
        Nothing -> pure $ reverse acc
        Just evt ->
          if predicate evt
            then drainMatching (evt : acc)
            else drainMatching acc -- Skip non-matching

-- | Get recent events from the log (for debug UI)
getRecentEvents ::
  EventBus someUpdate ->
  IO [someUpdate]
getRecentEvents bus = do
  eventLog' <- readTVarIO (eventLog bus)
  pure $ toList eventLog'

-- * Pattern matching

{- | Pattern match on specific update type

Note: Returns unsafe-coerced result since 'EventResult' is a type family.
This is safe because if the update matches, the result type must match too.
-}
matchUpdate ::
  forall event state.
  (UpdateEvent event, Typeable event, EventState event ~ state) =>
  SomeUpdate state ->
  Maybe (event, EventResult event)
matchUpdate (SomeUpdate evt result _timestamp) = do
  typedUpdate <- cast evt
  -- Safe: if update type matches, result type must match (type family relation)
  pure (typedUpdate, unsafeCoerce result)
