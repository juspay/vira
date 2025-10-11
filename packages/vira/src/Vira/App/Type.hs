{-# LANGUAGE DuplicateRecordFields #-}

-- | Core types for the Vira application
module Vira.App.Type (
  ViraRuntimeState (..),
) where

import Control.Concurrent.STM (TChan)
import Data.Acid (AcidState)
import Servant.Links (Link)
import Vira.App.InstanceInfo (InstanceInfo)
import Vira.Refresh.Type (RefreshState)
import Vira.State.Core (ViraState)
import Vira.Supervisor.Type (TaskSupervisor)
import Vira.Tool.Type.Tools (Tools)
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
  , -- Broadcast channel to track when state is updated
    stateUpdated :: TChan (Text, ByteString)
  , -- Cached tools data (mutable for refreshing)
    tools :: TVar Tools
  , -- Git repository auto-refresh state
    refreshState :: RefreshState
  }
