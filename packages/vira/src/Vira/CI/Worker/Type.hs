-- | Job worker types (split to avoid circular dependencies)
module Vira.CI.Worker.Type (
  JobWorkerState (..),
  newJobWorkerState,
) where

-- | Job worker state with concurrency configuration
newtype JobWorkerState = JobWorkerState
  { maxConcurrent :: Int
  -- ^ Maximum number of concurrent jobs allowed
  }

-- | Create new job worker state with default max concurrent jobs (3)
newJobWorkerState :: IO JobWorkerState
newJobWorkerState = pure $ JobWorkerState {maxConcurrent = 2}
