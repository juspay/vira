{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Event system for broadcasting acid-state updates
module Vira.Event.Type where

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)

-- | Generic event that can represent any acid-state action
data ViraEvent = ViraEvent
  { eventType :: String -- Type name as string (from show . typeOf)
  , eventData :: ByteString -- SafeCopy serialized action data
  }
  deriving stock (Generic, Show, Typeable, Eq)

-- | Event with timestamp
data TimestampedEvent = TimestampedEvent
  { eventTime :: UTCTime
  , event :: ViraEvent
  }
  deriving stock (Generic, Show, Typeable, Eq)

$(deriveSafeCopy 0 'base ''ViraEvent)
$(deriveSafeCopy 0 'base ''TimestampedEvent)
