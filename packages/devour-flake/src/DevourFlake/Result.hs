{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DevourFlake.Result (
  DevourFlakeResult (..),
  SystemOutputs (..),
) where

import Data.Aeson (FromJSON)
import Data.Map.Strict qualified as Map
import System.Nix.System (System)

-- | Represents the `result` JSON of devour-flake
newtype DevourFlakeResult = DevourFlakeResult
  { systems :: Map System SystemOutputs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Semigroup DevourFlakeResult where
  r1 <> r2 =
    DevourFlakeResult $ r1.systems `unionAppend` r2.systems
    where
      unionAppend = Map.unionWith (<>)

-- | Outputs for a single system
data SystemOutputs = SystemOutputs
  { byName :: Map Text FilePath
  , outPaths :: [FilePath]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Semigroup SystemOutputs where
  o1 <> o2 =
    SystemOutputs (o1.byName <> o2.byName) (o1.outPaths <> o2.outPaths)
