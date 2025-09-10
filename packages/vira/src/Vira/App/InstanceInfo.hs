{-# LANGUAGE OverloadedRecordDot #-}

-- | Instance information (hostname and platform) for the running Vira instance.
module Vira.App.InstanceInfo (
  InstanceInfo (..),
  getInstanceInfo,
  platform,
  instanceEmoji,
) where

import Network.HostName (getHostName)
import System.Info qualified as SysInfo

-- | Information about the current Vira instance
data InstanceInfo = InstanceInfo
  { hostname :: Text
  -- ^ The hostname of the machine running Vira
  , os :: Text
  -- ^ The operating system name
  , arch :: Text
  -- ^ The architecture name
  }
  deriving stock (Show, Eq)

-- | Get instance information from the system
getInstanceInfo :: IO InstanceInfo
getInstanceInfo = do
  hostName <- getHostName
  pure $
    InstanceInfo
      { hostname = toText hostName
      , os = toText SysInfo.os
      , arch = toText SysInfo.arch
      }

-- | Compute the platform string from InstanceInfo
platform :: InstanceInfo -> Text
platform instanceInfo = instanceInfo.os <> "/" <> instanceInfo.arch

-- | Get an emoji representing the operating system
instanceEmoji :: InstanceInfo -> Text
instanceEmoji instanceInfo = case instanceInfo.os of
  "linux" -> "🐧"
  "darwin" -> "🍎"
  _ -> "❓"
