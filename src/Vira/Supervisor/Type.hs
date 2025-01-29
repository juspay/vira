module Vira.Supervisor.Type where

import Effectful.Concurrent.Async (Async)
import System.Exit (ExitCode)
import Vira.State.Type (JobId)

type TaskId = JobId

data TaskState
  = Running
  | Finished ExitCode
  | Killed
  deriving stock (Generic, Show)

-- TODO Use ixset-typed
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
  , asyncHandle :: Async ExitCode
  -- ^ The `Async` handle for the task
  }
  deriving stock (Generic)
