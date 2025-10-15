-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog.Message (RichMessage)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, runLogActionStdout)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Effectful.Reader.Static qualified as ER
import Vira.App.CLI (GlobalSettings (..))
import Vira.App.Type (ViraRuntimeState)
import Prelude hiding (Reader, ask, asks, runReader)

type AppStack =
  '[ Reader ViraRuntimeState
   , Concurrent
   , Process
   , FileSystem
   , ER.Reader LogContext
   , Log (RichMessage IO)
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
