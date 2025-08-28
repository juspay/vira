{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Data.SafeCopy (SafeCopy)
import Data.Typeable (typeOf)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Stack (AppState (acid))
import Vira.Event.Broadcast qualified as Broadcast
import Vira.Event.Type (ViraEvent (ViraEvent))
import Vira.State.Core (ViraState)
import Prelude hiding (Reader, ask, asks, runReader)

-- Like `Acid.query`, but runs in effectful monad, whilst looking up the acid-state in Reader
query ::
  ( QueryEvent event
  , EventState event ~ ViraState
  , Reader AppState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
query event = do
  acid <- asks acid
  liftIO $ Acid.query acid event

-- Like `Acid.update`, but runs in effectful monad, whilst looking up the acid-state in Reader
-- Automatically publishes events for all update actions
update ::
  ( UpdateEvent event
  , EventState event ~ ViraState
  , SafeCopy event
  , Typeable event
  , Reader AppState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  acid <- asks acid
  result <- liftIO $ Acid.update acid event
  -- Publish event using typeOf (placeholder data for now)
  let eventType = show (typeOf event)
      eventData = "event-data-placeholder" -- Placeholder for actual serialization
      viraEvent = ViraEvent eventType eventData
  Broadcast.broadcastEvent viraEvent
  pure result

createCheckpoint :: (Reader AppState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
