-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Vira.App.CLI (GlobalSettings (..))
import Vira.App.Type (ViraRuntimeState)
import Vira.Lib.Logging (runLogActionStdout)
import Prelude hiding (Reader, ask, asks, runReader)

type AppStack =
  '[ Reader ViraRuntimeState
   , Concurrent
   , Process
   , FileSystem
   , Log Message
   , IOE
   ]

-- | Run the application stack in IO monad
runApp :: GlobalSettings -> ViraRuntimeState -> Eff AppStack a -> IO a
runApp globalSettings viraRuntimeState =
  do
    runEff
    . runLogActionStdout (logLevel globalSettings)
    . runFileSystem
    . runProcess
    . runConcurrent
    . runReader viraRuntimeState
