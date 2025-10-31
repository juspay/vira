{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Vira event system - Effectful-friendly wrappers and re-exports

This module provides:
- Re-exports of Core (generic event bus) and Entity (Vira-specific filtering)
- Effectful-friendly wrappers that use Reader ViraRuntimeState
-}
module Vira.App.Event (
  -- * Re-exports from Core (with constructors)
  module Vira.App.Event.Core,
  TimestampedUpdate (..),

  -- * Re-exports from Entity
  module Vira.App.Event.Entity,

  -- * Effectful wrappers
  publishUpdate,
  subscribe,
  getRecentEvents,
) where

import Control.Concurrent.STM (TChan)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App.Event.Core (TimestampedUpdate)
import Vira.App.Event.Core qualified as Core
import Vira.App.Event.Entity
import Vira.App.Type (ViraRuntimeState (..))

-- * Effectful wrappers

-- | Publish an update event to all subscribers (Effectful wrapper)
publishUpdate ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  ViraSomeUpdate ->
  Eff es ()
publishUpdate someUpdate = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.publishUpdate bus someUpdate

-- | Subscribe to events (Effectful wrapper)
subscribe ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es (TChan (TimestampedUpdate ViraSomeUpdate))
subscribe = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.subscribe bus

-- | Get recent events from the log (Effectful wrapper)
getRecentEvents ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es [TimestampedUpdate ViraSomeUpdate]
getRecentEvents = do
  bus <- Reader.asks (.eventBus)
  liftIO $ Core.getRecentEvents bus
