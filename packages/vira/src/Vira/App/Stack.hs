-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog.Message (RichMessage)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), runLogActionStdout)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Effectful.Reader.Static qualified as ER
import Vira.App.CLI (GlobalSettings (..))
import Vira.App.Type (ViraRuntimeState)
import Prelude hiding (Reader, ask, asks, runReader)

-- | Common effect stack (without ViraRuntimeState)
type AppStackCLI =
  '[ Concurrent
   , Process
   , FileSystem
   , Environment
   , ER.Reader LogContext
   , Log (RichMessage IO)
   , IOE
   ]

type AppStack = Reader ViraRuntimeState : AppStackCLI

-- | Run the application stack in IO monad
runApp :: GlobalSettings -> ViraRuntimeState -> Eff AppStack a -> IO a
runApp globalSettings viraRuntimeState =
  runAppCLI globalSettings . runReader viraRuntimeState

-- | Run common effect stack (for CLI, without ViraRuntimeState)
runAppCLI :: GlobalSettings -> Eff AppStackCLI a -> IO a
runAppCLI globalSettings =
  runEff
    . runLogActionStdout (logLevel globalSettings)
    . runEnvironment
    . runFileSystem
    . runProcess
    . runConcurrent
