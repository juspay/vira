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
  { tasks :: MVar (Map TaskId (Async ExitCode))
  -- ^ Current tasks, running or not
  , workDir :: FilePath
  -- ^ Base working directory for all tasks. This assigns `${workDir}/${taskId}/` as $PWD for each task.
  }
