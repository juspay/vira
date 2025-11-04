{-# LANGUAGE OverloadedRecordDot #-}

-- | Job worker types (split to avoid circular dependencies)
module Vira.CI.Worker.Type (
  JobWorkerState (..),
  newJobWorkerState,
) where

import Effectful.Colog.Simple (Severity)
import System.Nix.Config.Core (NixConfig (..), NixConfigField (..))

-- | Job worker state with concurrency configuration
data JobWorkerState = JobWorkerState
  { maxConcurrent :: Int
  -- ^ Maximum number of concurrent jobs allowed
  , minSeverity :: Severity
  -- ^ Minimum log severity for job output
  }

{- | Create job worker state from nix config

Reads max-jobs from nix config. Fails if max-jobs is 0 (auto) since we require explicit concurrency limit.
-}
newJobWorkerState :: NixConfig -> Severity -> JobWorkerState
newJobWorkerState nixCfg minSev =
  let jobs = nixCfg.maxJobs.value
      maxConcurrent =
        if jobs == 0
          then error "nix max-jobs = 0 (auto) - set explicit value"
          else fromIntegral jobs
   in JobWorkerState {maxConcurrent, minSeverity = minSev}
