{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Colog (Message, Severity (Info))
import Control.Concurrent.STM (writeTChan)
import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Type (ViraRuntimeState (acid, stateUpdated))
import Vira.Lib.Logging (log)
import Vira.State.Core (ViraState)
import Prelude hiding (Reader, ask, asks, runReader)

-- Like `Acid.query`, but runs in effectful monad, whilst looking up the acid-state in Reader
query ::
  ( QueryEvent event
  , EventState event ~ ViraState
  , Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
query event = do
  acid <- asks acid
  liftIO $ Acid.query acid event

{- | Like `Acid.update`, but runs in effectful monad, whilst looking up the acid-state in Reader

NOTE: This does NOT automatically broadcast events. Use `broadcastUpdate` explicitly
when you want to notify SSE listeners of entity changes.
-}
update ::
  ( UpdateEvent event
  , EventState event ~ ViraState
  , Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  acid <- asks acid
  liftIO (Acid.update acid event)

{- | Broadcast an entity-scoped update event to SSE listeners

Use this to notify clients when specific entities change. The event scope should be:
- "repo:<repoName>" for repository updates
- "job:<jobId>" for job updates

This triggers page reloads only for pages listening to that specific entity.
-}
broadcastUpdate ::
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  , Log Message :> es
  ) =>
  Text ->
  Eff es ()
broadcastUpdate eventScope = do
  stateUpdated <- asks @ViraRuntimeState stateUpdated
  liftIO $ atomically $ writeTChan stateUpdated (eventScope, mempty)
  log Info $ "📡 " <> eventScope

createCheckpoint :: (Reader ViraRuntimeState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
