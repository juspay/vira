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
  , schedulerLock :: MVar ()
  -- ^ Mutex for scheduling decisions (prevents race conditions)
  }

-- | Create job worker state with explicit max concurrent value
newJobWorkerState :: Int -> Severity -> IO JobWorkerState
newJobWorkerState maxConcurrent minSev = do
  lock <- newMVar () -- Create mutex in unlocked state
  pure $ JobWorkerState {maxConcurrent, minSeverity = minSev, schedulerLock = lock}
