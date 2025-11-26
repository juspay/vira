{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DevourFlake.Result (
  DevourFlakeResult (..),
  SystemOutputs (..),
  extractSystems,
) where

import Data.Aeson (FromJSON (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Nix.System (System)

-- | Represents the @result@ JSON of @devour-flake@
newtype DevourFlakeResult = DevourFlakeResult
  { systems :: Map System SystemOutputs
  -- ^ Map from 'System.Nix.System.System' to 'SystemOutputs'
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

instance Semigroup DevourFlakeResult where
  r1 <> r2 =
    DevourFlakeResult $ r1.systems `unionAppend` r2.systems
    where
      unionAppend = Map.unionWith (<>)

-- | Outputs for a single 'System.Nix.System.System'
data SystemOutputs = SystemOutputs
  { byName :: Map Text FilePath
  , outPaths :: [FilePath]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Semigroup SystemOutputs where
  o1 <> o2 =
    SystemOutputs (o1.byName <> o2.byName) (o1.outPaths <> o2.outPaths)

-- | Extract unique systems from multiple 'DevourFlakeResult's
extractSystems :: [DevourFlakeResult] -> Set System
extractSystems results =
  Set.fromList $ concatMap (Map.keys . (.systems)) results
