{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Remote builder resolution for multi-platform builds
module Vira.CI.Nix.RemoteBuilderResolver (
  -- * Resolution
  matchSystemToBuilder,
  resolveBuildsToRemoteBuilders,
  partitionByLocalSystem,

  -- * Local system detection
  getLocalSystem,

  -- * Types
  BuildTarget (..),
) where

import Colog.Message (RichMessage)

import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import System.Nix.Config (nixConfigShow)
import System.Nix.Config.Builders (RemoteBuilder (..), resolveBuilders)
import System.Nix.Config.Core (NixConfig (..), NixConfigField (..))
import System.Nix.System (System)

-- | A build target: either local or on a remote builder
data BuildTarget
  = -- | Build locally using devour-flake
    LocalBuild System
  | -- | Build on a remote builder via SSH
    RemoteBuild System RemoteBuilder
  deriving stock (Show, Eq)

-- | Get the local system from Nix configuration
getLocalSystem ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  Eff es System
getLocalSystem = do
  config <- nixConfigShow
  pure config.system.value

-- | Match a system to an available remote builder
matchSystemToBuilder :: [RemoteBuilder] -> System -> Maybe RemoteBuilder
matchSystemToBuilder builders sys =
  find (\b -> sys `elem` b.platforms) builders

-- | Resolve systems to remote builders, returning a map of matches
resolveBuildsToRemoteBuilders ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  [System] ->
  Eff es (Map System RemoteBuilder)
resolveBuildsToRemoteBuilders systems = do
  config <- nixConfigShow
  builders <- resolveBuilders config.builders.value
  pure $
    Map.fromList
      [ (sys, builder)
      | sys <- systems
      , Just builder <- [matchSystemToBuilder builders sys]
      ]

-- | Partition systems into local and remote build targets
partitionByLocalSystem ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  [System] ->
  Eff es [BuildTarget]
partitionByLocalSystem systems = do
  config <- nixConfigShow
  let localSys = config.system.value
      extraPlatforms = config.extraPlatforms.value
      locallyBuildable = localSys : extraPlatforms
  builders <- resolveBuilders config.builders.value
  pure
    [ if sys `elem` locallyBuildable
        then LocalBuild sys
        else case matchSystemToBuilder builders sys of
          Just builder -> RemoteBuild sys builder
          -- Fall back to local build if no remote builder found
          -- (Nix will handle the error if it can't actually build)
          Nothing -> LocalBuild sys
    | sys <- systems
    ]
