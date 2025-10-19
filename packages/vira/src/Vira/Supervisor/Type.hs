module Vira.Supervisor.Type where

import Effectful.Concurrent.Async (Async)
import System.Exit (ExitCode)
import System.Tail (Tail)
import Vira.State.Type (JobId)

type TaskId = JobId

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
  { info :: TaskInfo
  -- ^ Task information
  , asyncHandle :: Async ()
  -- ^ The `Async` handle for the task
  }
  deriving stock (Generic)

data TaskInfo = TaskInfo
  { taskId :: TaskId
  -- ^ Unique identifier for the task
  , workDir :: FilePath
  -- ^ Working directory of this task
  , tailHandle :: Tail
  -- ^ The tail handle for log streaming
  }
  deriving stock (Generic)

{- | Task was explicitly terminated (by the user)
Used by killTask
-}
data Terminated = Terminated
  deriving stock (Show)

instance Exception Terminated where
  displayException Terminated = "Task was killed by user"
