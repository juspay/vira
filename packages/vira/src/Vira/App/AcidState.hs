{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Control.Concurrent.STM.TChan (TChan)
import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Data.Acid.Events (SomeUpdate)
import Data.Acid.Events qualified as Events
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.State.Core (ViraState)
import Prelude hiding (Reader, ask, asks, runReader)

-- | Like 'Data.Acid.query', but runs in 'Effectful' monad, whilst looking up the acid-state in 'Effectful.Reader.Dynamic.Reader'
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

{- | Like 'Data.Acid.update', but runs in 'Effectful' monad, whilst looking up the acid-state in 'Effectful.Reader.Dynamic.Reader'

AUTOMATICALLY publishes events to the 'Data.Acid.Events.EventBus'.
-}
update ::
  forall event es.
  ( UpdateEvent event
  , EventState event ~ ViraState
  , Show event
  , Typeable event
  , Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  acid <- asks acid
  bus <- asks @ViraRuntimeState eventBus
  liftIO $ Events.update acid bus event

-- | Subscribe to 'Data.Acid.Events.EventBus' for receiving 'Data.Acid.Events.SomeUpdate' events
subscribe ::
  forall es.
  ( Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es (TChan (SomeUpdate ViraState))
subscribe = do
  bus <- asks @ViraRuntimeState eventBus
  liftIO $ Events.subscribe bus

createCheckpoint :: (Reader ViraRuntimeState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
