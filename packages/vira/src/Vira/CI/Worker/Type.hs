{-# LANGUAGE OverloadedRecordDot #-}

-- | Job worker types (split to avoid circular dependencies)
module Vira.CI.Worker.Type (
  JobWorkerState (..),
  newJobWorkerState,

  -- * Job completion callbacks
  JobCallback,
  registerJobCallback,
  takeJobCallback,
) where

import Data.Map.Strict qualified as Map
import Effectful.Colog.Simple (Severity)
import Vira.State.Type (JobId, JobResult)

-- | Callback invoked when a job completes
type JobCallback = JobResult -> IO ()

-- | Job worker state with concurrency configuration
data JobWorkerState = JobWorkerState
  { maxConcurrent :: Int
  -- ^ Maximum number of concurrent jobs allowed
  , minSeverity :: Severity
  -- ^ Minimum log severity for job output
  , schedulerLock :: MVar ()
  -- ^ Mutex for scheduling decisions (prevents race conditions)
  , jobCallbacks :: TVar (Map JobId JobCallback)
  -- ^ Registered callbacks for job completion (in-memory only, not persisted)
  }

-- | Create job worker state with explicit max concurrent value
newJobWorkerState :: Int -> Severity -> IO JobWorkerState
newJobWorkerState maxConcurrent minSev = do
  lock <- newMVar () -- Create mutex in unlocked state
  callbacks <- newTVarIO Map.empty
  pure $
    JobWorkerState
      { maxConcurrent
      , minSeverity = minSev
      , schedulerLock = lock
      , jobCallbacks = callbacks
      }

-- | Register a callback to be invoked when a job completes
registerJobCallback :: JobWorkerState -> JobId -> JobCallback -> IO ()
registerJobCallback jws jobId callback =
  atomically $ modifyTVar' jws.jobCallbacks $ Map.insert jobId callback

-- | Take and remove a callback for a job (if registered)
takeJobCallback :: JobWorkerState -> JobId -> IO (Maybe JobCallback)
takeJobCallback jws jobId = atomically $ do
  m <- readTVar jws.jobCallbacks
  writeTVar jws.jobCallbacks $ Map.delete jobId m
  pure $ Map.lookup jobId m
