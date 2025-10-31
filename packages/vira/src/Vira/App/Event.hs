{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Vira event system - Effectful-friendly wrappers and re-exports

This module provides:
- Re-exports of Core (generic event bus) and Entity (Vira-specific filtering)
- Effectful-friendly wrappers that use Reader ViraRuntimeState
-}
module Vira.App.Event (
  -- * Re-exports from Core (with constructors)
  SomeUpdate (..),
  TimestampedUpdate (..),
  EventBus (..),
  newEventBus,
  matchUpdate,

  -- * Re-exports from Entity
  module Vira.App.Event.Entity,

  -- * Effectful wrappers
  publishUpdate,
  subscribe,
  getRecentEvents,
) where

import Control.Concurrent.STM (TChan)
import Data.Acid.Events (EventBus (..), SomeUpdate (..), TimestampedUpdate (..), matchUpdate, newEventBus)
import Data.Acid.Events qualified as Core
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Event.Entity
import Vira.App.Type (ViraRuntimeState (..))
import Vira.State.Type (ViraState)

-- * Effectful wrappers

-- | Publish an update event to all subscribers (Effectful wrapper)
publishUpdate ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  SomeUpdate ViraState AffectedEntities ->
  Eff es ()
publishUpdate someUpdate = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.publishUpdate bus someUpdate

-- | Subscribe to events (Effectful wrapper)
subscribe ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es (TChan (TimestampedUpdate (SomeUpdate ViraState AffectedEntities)))
subscribe = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.subscribe bus

-- | Get recent events from the log (Effectful wrapper)
getRecentEvents ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es [TimestampedUpdate (SomeUpdate ViraState AffectedEntities)]
getRecentEvents = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.getRecentEvents bus
