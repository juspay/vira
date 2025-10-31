{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Event (AffectedEntities, SomeUpdate (..))
import Vira.App.Event qualified as Event
import Vira.App.Event.Instances ()
import Vira.App.Type (ViraRuntimeState (acid))
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

Now AUTOMATICALLY publishes events to the event bus for Updates with AffectedEntities instance.
This enables SSE updates, event subscriptions, and debug logging without manual broadcasting.
-}
update ::
  ( UpdateEvent event
  , EventState event ~ ViraState
  , AffectedEntities event
  , Show event
  , Typeable event
  , Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  acid <- asks acid
  result <- liftIO (Acid.update acid event)

  -- Auto-publish event to bus
  Event.publishUpdate (SomeUpdate event result)

  pure result

createCheckpoint :: (Reader ViraRuntimeState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
