module Vira.Supervisor.Type where

import Effectful.Concurrent.Async
import System.Exit (ExitCode)

type TaskId = Int
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

data TaskSupervisor = TaskSupervisor
  { tasks :: MVar (Map TaskId (Async TaskOutput))
  , dummy :: ()
  }
