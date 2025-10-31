{-# LANGUAGE DuplicateRecordFields #-}

-- | Core types for the Vira application
module Vira.App.Type (
  ViraRuntimeState (..),
) where

import Data.Acid (AcidState)
import Data.Acid.Events (EventBus, SomeUpdate)
import Data.Time (UTCTime)
import Servant.Links (Link)
import System.Nix.Cache.Keys (PublicKey)
import Vira.App.Event.Entity (AffectedEntities)
import Vira.App.InstanceInfo (InstanceInfo)
import Vira.Environment.Tool.Type.Tools (Tools)
import Vira.Refresh.Type (RefreshState)
import Vira.State.Core (ViraState)
import Vira.Supervisor.Type (TaskSupervisor)
import Vira.Web.LinkTo.Type (LinkTo)

-- | Application-wide state available in Effectful stack
data ViraRuntimeState = ViraRuntimeState
  { -- Instance information (hostname, platform)
    instanceInfo :: InstanceInfo
  , -- The state of the app
    acid :: AcidState ViraState
  , -- Process supervisor state
    supervisor :: TaskSupervisor
  , -- Create a link to a part of the app.
    --
    -- This is decoupled from servant types deliberately to avoid cyclic imports.
    linkTo :: LinkTo -> Link
  , -- Event bus for all Update events (SSE, subscriptions, debug log)
    eventBus :: EventBus (SomeUpdate ViraState AffectedEntities)
  , -- Cached tools data (mutable for refreshing)
    tools :: TVar Tools
  , -- Git repository auto-refresh state
    refreshState :: RefreshState
  , -- Server start time for uptime tracking
    startTime :: UTCTime
  , -- Cache public key (for UI display)
    cachePublicKey :: PublicKey
  }
