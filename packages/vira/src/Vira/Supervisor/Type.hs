module Vira.Supervisor.Type where

import Effectful.Concurrent.Async (Async)
import LogSink.Broadcast (Broadcast)
import System.Exit (ExitCode)
import Vira.State.Type (JobId)

type TaskId = JobId

data TaskState
  = Running
  | Finished ExitCode
  | Killed
  deriving stock (Eq, Generic, Show)

{- | Supervisor for managing 'Task's

TODO Use ixset-typed
-}
data TaskSupervisor = TaskSupervisor
  { tasks :: MVar (Map TaskId Task)
  -- ^ Current 'Task's, running or not
  , baseWorkDir :: FilePath
  -- ^ Base working directory for all tasks. This assigns @${workDir}/${taskId}/@ as @$PWD@ for each task.
  }
  deriving stock (Generic)

-- | A task managed by the 'TaskSupervisor'
data Task = Task
  { info :: TaskInfo
  -- ^ 'TaskInfo' for this task
  , asyncHandle :: Async ()
  -- ^ The 'Effectful.Concurrent.Async.Async' handle for the task
  }
  deriving stock (Generic)

-- | Task metadata and handles
data TaskInfo = TaskInfo
  { taskId :: TaskId
  -- ^ Unique 'TaskId' for the task
  , workDir :: FilePath
  -- ^ Working directory of this task
  , broadcast :: Broadcast Text
  -- ^ In-memory broadcast for SSE streaming
  }
  deriving stock (Generic)

{- | Exception thrown when a 'Task' is explicitly terminated (by the user)

Used by 'Vira.Supervisor.Core.killTask'
-}
data Terminated = Terminated
  deriving stock (Show)

instance Exception Terminated where
  displayException Terminated = "Task was killed by user"
