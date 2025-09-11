{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline where

import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (cwd), env, proc)
import System.GHSignoff
import System.Which (staticWhich)
import Vira.Lib.Attic
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Core qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..))

{- | Path to the `mkdir` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
mkdir :: FilePath
mkdir = $(staticWhich "mkdir")

-- | Get all build stages for a CI pipeline
getStages :: St.Repo -> St.Branch -> Maybe CachixSettings -> Maybe AtticSettings -> NonEmpty CreateProcess
getStages repo branch mCachix mAttic = do
  stageCreateProjectDir
    :| one stagesClone
    <> maybe [] (one . stageAtticLogin) mAttic
    <> [stageBuild]
    <> (maybe mempty (one . stageCachixPush) mCachix <> maybe mempty (one . stageAtticPush) mAttic)
    <> [stageGHSignoff]
  where
    stageCreateProjectDir =
      proc mkdir ["project"]
    stagesClone =
      Git.cloneAtCommit repo.cloneUrl branch.headCommit
        & \p -> p {cwd = Just "project"}
    stageBuild =
      Omnix.omnixCiProcess
        { cwd = Just "project"
        }
    -- Run the stage before any other attic processes
    stageAtticLogin attic =
      (atticLoginProcess attic.atticServer attic.atticToken)
        { cwd = Just "project"
        }
    stageCachixPush cachix =
      (cachixPushProcess cachix.cachixName "result")
        { env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]
        , cwd = Just "project"
        }
    stageAtticPush attic =
      (atticPushProcess attic.atticServer attic.atticCacheName "result")
        { cwd = Just "project"
        }
    stageGHSignoff =
      (ghSignoffProcess "vira" "ci")
        { cwd = Just "project"
        }
