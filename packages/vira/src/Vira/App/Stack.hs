-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Servant (Handler (Handler), ServerError)
import Vira.App.CLI (GlobalSettings (..), WebSettings)
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

type AppServantStack = (Error ServerError : Reader WebSettings : AppStack)

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

-- | Like `runApp`, but for Servant 'Handler'.
runAppInServant :: GlobalSettings -> ViraRuntimeState -> WebSettings -> Eff AppServantStack a -> Handler a
runAppInServant globalSettings viraRuntimeState webSettings =
  Handler . ExceptT . runApp globalSettings viraRuntimeState . runReader webSettings . runErrorNoCallStack
