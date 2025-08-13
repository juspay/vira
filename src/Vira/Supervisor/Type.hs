module Vira.Supervisor.Type where

import Effectful.Concurrent.Async (Async)
import System.Exit (ExitCode)
import Vira.Lib.FileTailer (FileTailer, FileTailerConfig, defaultConfig)
import Vira.State.Type (JobId)

type TaskId = JobId

{- | Standard configuration for Vira's log file tailer
100 recent lines, batches of up to 50 lines, client queues of up to 100 batches
-}
logFileTailerConfig :: FileTailerConfig
logFileTailerConfig = defaultConfig

type LogFileTailer = FileTailer

data TaskState
  = Running
  | Finished ExitCode
  | Killed
  deriving stock (Eq, Generic, Show)

{- | Supervisor for managing tasks

TODO Use ixset-typed
-}
data TaskSupervisor = TaskSupervisor
  { tasks :: MVar (Map TaskId Task)
  -- ^ Current tasks, running or not
  , baseWorkDir :: FilePath
  -- ^ Base working directory for all tasks. This assigns `${workDir}/${taskId}/` as $PWD for each task.
  }
  deriving stock (Generic)

-- | A task managed by the supervisor
data Task = Task
  { workDir :: FilePath
  -- ^ Working directory of this task
  , asyncHandle :: Async (Either TaskException ExitCode)
  -- ^ The `Async` handle for the task
  , logTailer :: MVar (Maybe LogFileTailer)
  -- ^ Shared log tailer, created on first client connection
  }
  deriving stock (Generic)

-- | Exceptions that occurred during a task
data TaskException
  = KilledByUser
  deriving stock (Show)

instance Exception TaskException where
  displayException KilledByUser = "Task was killed by user"
