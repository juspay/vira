{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Effectful stack for our app.
module Vira.App.AcidState where

import Control.Concurrent.STM (atomically, writeTChan)
import Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent)
import Data.Acid qualified as Acid
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Stack (AppState (acid, stateUpdated))
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
-- Also records the update timestamp in the stateUpdated broadcast channel
update ::
  ( UpdateEvent event
  , EventState event ~ ViraState
  , Reader AppState :> es
  , IOE :> es
  ) =>
  event ->
  Eff es (EventResult event)
update event = do
  appState <- asks @AppState id
  result <- liftIO $ Acid.update (acid appState) event
  -- Record the update timestamp
  now <- liftIO getCurrentTime
  liftIO $ Control.Concurrent.STM.atomically $ writeTChan (stateUpdated appState) now
  putStrLn "📝 State updated, notified listeners"
  pure result

createCheckpoint :: (Reader AppState :> es, IOE :> es) => Eff es ()
createCheckpoint = do
  acid <- asks acid
  liftIO $ Acid.createCheckpoint acid
