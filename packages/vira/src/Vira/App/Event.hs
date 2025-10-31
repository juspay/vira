{-# LANGUAGE OverloadedRecordDot #-}

-- | General event system using acid-state Update types directly
module Vira.App.Event (
  -- * Re-exports
  module Vira.App.Event.Type,

  -- * Event bus operations
  publishUpdate,
  subscribe,
  getRecentEvents,

  -- * Pattern matching
  matchUpdate,

  -- * Entity filtering helpers (for SSE)
  affectsRepo,
  affectsJob,
  affectsAnyJob,
) where

import Control.Concurrent.STM (TChan, dupTChan, writeTChan)
import Data.Acid (EventResult, UpdateEvent)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Time (getCurrentTime)
import Data.Typeable (cast)
import Effectful (Eff, IOE, (:>))
import Effectful.Git (RepoName)
import Effectful.Reader.Dynamic qualified as Reader
import Unsafe.Coerce (unsafeCoerce)
import Vira.App.Event.Type
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Lib.STM (drainRemainingTChan)
import Vira.State.Type (JobId)

-- * Event bus operations

-- | Publish an update event to all subscribers
publishUpdate ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  SomeUpdate ->
  Eff es ()
publishUpdate someUpdate = do
  bus <- Reader.asks (.eventBus)
  now <- liftIO getCurrentTime
  let timestamped = TimestampedUpdate now someUpdate

  liftIO $ atomically $ do
    -- Publish to subscribers
    writeTChan bus.channel timestamped
    -- Append to circular buffer log
    modifyTVar' bus.eventLog $ \log ->
      let newLog = log |> timestamped
       in if Seq.length newLog > bus.maxLogSize
            then Seq.drop 1 newLog
            else newLog

-- | Subscribe to events (returns duplicate channel starting from now)
subscribe ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es (TChan TimestampedUpdate)
subscribe = do
  bus <- Reader.asks (.eventBus)
  liftIO $ atomically $ do
    dup <- dupTChan bus.channel
    void $ drainRemainingTChan dup -- Start fresh
    pure dup

-- | Get recent events from the log (for debug UI)
getRecentEvents ::
  ( Reader.Reader ViraRuntimeState :> es
  , IOE :> es
  ) =>
  Eff es [TimestampedUpdate]
getRecentEvents = do
  bus <- Reader.asks (.eventBus)
  log <- liftIO $ readTVarIO bus.eventLog
  pure $ toList log

-- * Pattern matching

{- | Pattern match on specific update type

Note: Returns unsafe-coerced result since EventResult is a type family.
This is safe because if the update matches, the result type must match too.
-}
matchUpdate ::
  forall event.
  (UpdateEvent event, Typeable event) =>
  SomeUpdate ->
  Maybe (event, EventResult event)
matchUpdate (SomeUpdate update result) = do
  typedUpdate <- cast update
  -- Safe: if update type matches, result type must match (type family relation)
  pure (typedUpdate, unsafeCoerce result)

-- * Entity filtering helpers

-- | Check if update affects a specific repo
affectsRepo :: RepoName -> SomeUpdate -> Bool
affectsRepo name (SomeUpdate update result) =
  name `elem` affectedRepos update result

-- | Check if update affects a specific job
affectsJob :: JobId -> SomeUpdate -> Bool
affectsJob jobId (SomeUpdate update result) =
  jobId `elem` affectedJobs update result

-- | Check if update affects any job
affectsAnyJob :: SomeUpdate -> Bool
affectsAnyJob (SomeUpdate update result) =
  not $ null $ affectedJobs update result
