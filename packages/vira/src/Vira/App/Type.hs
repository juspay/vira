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
import Vira.App.InstanceInfo (InstanceInfo)
import Vira.CI.Worker.Type (JobWorkerState)
import Vira.Environment.Tool.Type.Tools (Tools)
import Vira.Refresh.Type (RefreshState)
import Vira.State.Core (ViraState)
import Vira.Supervisor.Type (TaskSupervisor)
import Vira.Web.LinkTo.Type (LinkTo)

-- | Application-wide state available in 'Effectful.Eff' stack
data ViraRuntimeState = ViraRuntimeState
  { instanceInfo :: InstanceInfo
  -- ^ Instance information (hostname, platform)
  , acid :: AcidState ViraState
  -- ^ The 'ViraState' managed by acid-state
  , supervisor :: TaskSupervisor
  -- ^ Process supervisor state
  , linkTo :: LinkTo -> Link
  {- ^ Create a link to a part of the app.

  This is decoupled from Servant types deliberately to avoid cyclic imports.
  -}
  , eventBus :: EventBus (SomeUpdate ViraState)
  -- ^ 'EventBus' for all 'Data.Acid.Events.SomeUpdate' events (SSE, subscriptions, debug log)
  , tools :: TVar Tools
  -- ^ Cached 'Tools' data (mutable for refreshing)
  , refreshState :: RefreshState
  -- ^ Git repository auto-refresh state
  , jobWorker :: JobWorkerState
  -- ^ CI job worker state (queue processing)
  , startTime :: UTCTime
  -- ^ Server start time for uptime tracking
  , cachePublicKey :: PublicKey
  -- ^ Cache 'PublicKey' (for UI display)
  }
