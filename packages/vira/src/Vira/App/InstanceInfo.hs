{-# LANGUAGE OverloadedRecordDot #-}

-- | Instance information (hostname and platform) for the running Vira instance.
module Vira.App.InstanceInfo (
  InstanceInfo (..),
  getInstanceInfo,
  platform,
) where

import Network.HostName (getHostName)
import System.Info qualified as SysInfo

-- | Information about the current Vira instance
data InstanceInfo = InstanceInfo
  { hostname :: Text
  -- ^ The hostname of the machine running Vira
  , os :: Text
  -- ^ The operating system name (from 'System.Info.os')
  , arch :: Text
  -- ^ The architecture name (from 'System.Info.arch')
  }
  deriving stock (Show, Eq)

-- | Get 'InstanceInfo' from the system
getInstanceInfo :: IO InstanceInfo
getInstanceInfo = do
  hostName <- getHostName
  pure $
    InstanceInfo
      { hostname = toText hostName
      , os = toText SysInfo.os
      , arch = toText SysInfo.arch
      }

-- | Compute the platform string from 'InstanceInfo'
platform :: InstanceInfo -> Text
platform instanceInfo = instanceInfo.os <> "/" <> instanceInfo.arch
