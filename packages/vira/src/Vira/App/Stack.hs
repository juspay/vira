-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Control.Concurrent.STM (TChan)
import Data.Acid (AcidState)
import Data.ByteString
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Servant (Handler (Handler), ServerError)
import Servant.Links (Link)
import Vira.App.CLI (CLISettings)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.Lib.Logging (runLogActionStdout)
import Vira.State.Core (ViraState)
import Vira.Supervisor.Type (TaskSupervisor)
import Prelude hiding (Reader, ask, asks, runReader)

type AppStack =
  '[ Reader AppState
   , Concurrent
   , Process
   , FileSystem
   , Log Message
   , IOE
   ]

type AppServantStack = (Error ServerError : AppStack)

-- | Run the application stack in IO monad
runApp :: AppState -> Eff AppStack a -> IO a
runApp cfg =
  do
    runEff
    . runLogActionStdout
    . runFileSystem
    . runProcess
    . runConcurrent
    . runReader cfg

-- | Like `runApp`, but for Servant 'Handler'.
runAppInServant :: AppState -> Eff AppServantStack a -> Handler a
runAppInServant cfg =
  Handler . ExceptT . runApp cfg . runErrorNoCallStack

-- | Application-wide state available in Effectful stack
data AppState = AppState
  { -- CLI args passed by the user
    cliSettings :: CLISettings
  , -- The state of the app
    acid :: AcidState ViraState
  , -- Process supervisor state
    supervisor :: TaskSupervisor
  , -- Create a link to a part of the app.
    --
    -- This is decoupled from servant types deliberately to avoid cyclic imports.
    linkTo :: LinkTo -> Link
  , -- Broadcast channel to track when state is updated
    stateUpdated :: TChan (Text, ByteString)
  , -- Next available stream ID for SSE connections
    nextAvailableID :: TVar Int
  }
