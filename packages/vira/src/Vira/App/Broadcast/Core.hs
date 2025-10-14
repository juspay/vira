{- |
Entity-scoped broadcast mechanism for SSE (Server-Sent Events).

This module provides a type-safe way to broadcast entity-scoped update events
to connected clients. Events are scoped to specific entities (e.g., "repo:my-repo",
"job:123") so that only pages listening to that entity will reload.

= Usage Pattern

Producer side (broadcasting updates):

@
broadcastUpdate (RepoScope "my-repo")
broadcastUpdate (JobScope 123)
@

Consumer side (listening for updates):

@
-- 1. Subscribe: create a duplicate channel for this listener
chanDup <- subscribeToBroadcasts

-- 2. Consume: read pending events from the duplicate
scopes <- consumeBroadcasts chanDup
@

The subscription model uses 'dupTChan' to allow multiple independent listeners.
Each listener gets their own duplicate channel starting from "now" (old messages drained).
-}
module Vira.App.Broadcast.Core (
  -- * Broadcasting
  broadcastUpdate,
  subscribeToBroadcasts,
  consumeBroadcasts,
) where

import Colog (Severity (Debug, Info))
import Colog.Message (RichMessage)
import Control.Concurrent.STM (dupTChan, writeTChan)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Broadcast.Type (BroadcastScope, UpdateBroadcast)
import Vira.App.Type (ViraRuntimeState (updateBroadcast))
import Vira.Lib.Logging (log)
import Vira.Lib.STM (drainRemainingTChan, drainTChan)
import Prelude hiding (Reader, ask, asks, runReader)

{- | Broadcast an entity-scoped update event to SSE listeners

Use this to notify clients when specific entities change:
- JobScope for job updates
- RepoScope for repository updates

This triggers page reloads only for pages listening to that specific entity.
-}
broadcastUpdate ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  ) =>
  BroadcastScope ->
  Eff es ()
broadcastUpdate scope = do
  chan <- asks @ViraRuntimeState updateBroadcast
  liftIO $ atomically $ writeTChan chan scope
  log Info $ "📡 " <> show scope

{- | Subscribe to broadcast updates by creating a duplicate channel

Creates an independent read-only view of the broadcast channel for a new listener.
Old messages are drained so the listener only receives updates from "now" onwards.

This should be called once per listener (e.g., once per SSE connection).
-}
subscribeToBroadcasts ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es UpdateBroadcast
subscribeToBroadcasts = do
  chan <- asks @ViraRuntimeState updateBroadcast
  liftIO $ atomically $ do
    dup <- dupTChan chan
    void $ drainRemainingTChan dup
    pure dup

{- | Consume all pending broadcast events from a duplicate channel

Drains all events currently in the channel and returns them as a list.
This is non-blocking - returns immediately with whatever events are available.

Each event is logged at Debug level.
-}
consumeBroadcasts ::
  ( IOE :> es
  , Log (RichMessage IO) :> es
  ) =>
  UpdateBroadcast ->
  Eff es [BroadcastScope]
consumeBroadcasts chan = do
  scopes <- liftIO $ atomically $ drainTChan chan
  forM (toList scopes) $ \scope -> do
    log Debug $ "Broadcast event received: scope=" <> show scope
    pure scope
