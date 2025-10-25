{- |
Entity-scoped broadcast mechanism for SSE (Server-Sent Events).

This module provides a type-safe way to broadcast entity-scoped update events
to connected clients. Events are scoped to specific entities (e.g., "repo:my-repo",
"job:123") so that only pages listening to that entity will reload.

= Usage Pattern

Producer side (broadcasting updates):

@
broadcastUpdate (RepoScope "my-repo")
broadcastUpdate (JobScope (Just 123))
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

import Colog (Severity (Debug))
import Colog.Message (RichMessage)
import Control.Concurrent.STM (dupTChan, writeTChan)
import Control.Concurrent.STM qualified as STM
import Data.List.NonEmpty qualified as NE
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.Broadcast.Type (BroadcastScope, UpdateBroadcast)
import Vira.App.Type (ViraRuntimeState (updateBroadcast))
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
  , ER.Reader LogContext :> es
  ) =>
  BroadcastScope ->
  Eff es ()
broadcastUpdate scope = do
  chan <- asks @ViraRuntimeState updateBroadcast
  liftIO $ STM.atomically $ writeTChan chan scope
  log Debug $ "📡 Broadcasting scope: " <> show scope

{- | Subscribe to broadcast updates by creating a duplicate channel

Creates an independent read-only view of the broadcast channel for a new listener.
Old messages are drained so the listener only receives updates from "now" onwards.

This should be called once per listener (e.g., once per SSE connection).

Note: Draining old messages on subscribe is safe because SSE streams use heartbeats
(see "Vira.Web.Stream.KeepAlive") to prevent connection timeouts. Under normal
operation, connections stay alive indefinitely, so draining only affects the initial
connection (no messages yet) and intentional reconnections (user refreshes page).
-}
subscribeToBroadcasts ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es UpdateBroadcast
subscribeToBroadcasts = do
  chan <- asks @ViraRuntimeState updateBroadcast
  liftIO $ STM.atomically $ do
    dup <- dupTChan chan
    void $ drainRemainingTChan dup
    pure dup

{- | Consume broadcast events from a duplicate channel with debouncing

Blocks until at least one event arrives, then waits 2.5 seconds to batch additional events.
This debouncing prevents rapid page reloads when multiple broadcasts occur in quick succession.

Returns all events collected during the debounce window (guaranteed non-empty).
-}
consumeBroadcasts ::
  ( IOE :> es
  , Concurrent :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  UpdateBroadcast ->
  Eff es (NonEmpty BroadcastScope)
consumeBroadcasts chan = do
  -- Block until first event arrives
  firstBatch <- liftIO $ STM.atomically $ drainTChan chan
  -- Debounce: wait for more events to batch together
  threadDelay 2_500_000 -- 2.5 seconds
  moreScopes <- liftIO $ STM.atomically $ drainRemainingTChan chan
  let allScopes = NE.appendList firstBatch moreScopes
  log Debug $ "Consumed " <> show (length allScopes) <> " broadcast events (initial=" <> show (length firstBatch) <> " batched=" <> show (length moreScopes) <> ")"
  forM allScopes $ \scope -> do
    log Debug $ "  - scope=" <> show scope
    pure scope
