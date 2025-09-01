{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Colog (Message, Severity (Info))
import Control.Concurrent.STM (writeTChan)
import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Data.SafeCopy (SafeCopy, safePut)
import Data.Serialize (runPut)
import Data.Typeable (typeOf)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Stack (AppState (acid, stateUpdated))
import Vira.Lib.Logging (log)
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
-- Also records the serialized update event in the stateUpdated broadcast channel
update ::
  ( UpdateEvent event
  , EventState event ~ ViraState
  , SafeCopy event
  , Typeable event
  , Reader AppState :> es
  , IOE :> es
  , Log Message :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  appState <- asks @AppState id
  result <- liftIO $ Acid.update (acid appState) event
  -- Serialize event name and data for logging
  let eventName = show $ typeOf event
      eventData = runPut $ safePut event
  liftIO $ atomically $ writeTChan (stateUpdated appState) (eventName, eventData)
  log Info $ "📝 State updated (" <> eventName <> "), notified listeners"
  pure result

createCheckpoint :: (Reader AppState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
