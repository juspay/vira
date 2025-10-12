-- | Broadcast channel type for entity-scoped SSE events
module Vira.App.Broadcast.Type (
  UpdateBroadcast,
) where

import Control.Concurrent.STM (TChan)

-- | Broadcast channel for entity-scoped update events
type UpdateBroadcast = TChan Text
