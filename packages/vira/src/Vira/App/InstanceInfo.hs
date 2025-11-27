{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Instance information (hostname and platform) for the running Vira instance.
module Vira.App.InstanceInfo (
  InstanceInfo (..),
  getInstanceInfo,
  platform,
) where

import Effectful.Git.Types (CommitID)
import Network.HostName (getHostName)
import System.Info qualified as SysInfo
import Vira.App.GitRev qualified as GitRev

-- | Information about the current Vira instance
data InstanceInfo = InstanceInfo
  { hostname :: Text
  -- ^ The hostname of the machine running Vira
  , os :: Text
  -- ^ The operating system name (from 'System.Info.os')
  , arch :: Text
  -- ^ The architecture name (from 'System.Info.arch')
  , commitId :: Maybe CommitID
  -- ^ Git commit ID from Nix build wrapper (Nothing in dev)
  }
  deriving stock (Show, Eq)

-- | Get 'InstanceInfo' from the system
getInstanceInfo :: IO InstanceInfo
getInstanceInfo = do
  hostName <- getHostName
  commit <- GitRev.getCommitId
  pure $
    InstanceInfo
      { hostname = toText hostName
      , os = toText SysInfo.os
      , arch = toText SysInfo.arch
      , commitId = commit
      }

-- | Compute the platform string from 'InstanceInfo'
platform :: InstanceInfo -> Text
platform instanceInfo = instanceInfo.os <> "/" <> instanceInfo.arch
