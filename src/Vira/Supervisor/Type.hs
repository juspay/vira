module Vira.Supervisor.Type where

import Effectful.Concurrent.Async
import System.Exit (ExitCode)
import Vira.State.Type (JobId)

type TaskId = JobId

data TaskOutput = TaskOutput
  { output :: String -- stdout/stderr
  , exitCode :: ExitCode
  }
  deriving stock (Generic, Show)

data TaskState
  = Running
  | Finished TaskOutput
  | Killed
  deriving stock (Generic, Show)

-- TODO Use ixset-typed
data TaskSupervisor = TaskSupervisor
  { tasks :: MVar (Map TaskId (Async TaskOutput))
  , dummy :: ()
  }
