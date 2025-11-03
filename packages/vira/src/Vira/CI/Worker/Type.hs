-- | Job worker types (split to avoid circular dependencies)
module Vira.CI.Worker.Type (
  JobWorkerState (..),
  newJobWorkerState,
) where

import Effectful.Colog.Simple (Severity)

-- | Job worker state with concurrency configuration
data JobWorkerState = JobWorkerState
  { maxConcurrent :: Int
  -- ^ Maximum number of concurrent jobs allowed
  , minSeverity :: Severity
  -- ^ Minimum log severity for job output
  }

-- | Create new job worker state with default max concurrent jobs (2) and severity (Info)
newJobWorkerState :: Severity -> JobWorkerState
newJobWorkerState minSev = JobWorkerState {maxConcurrent = 2, minSeverity = minSev}
