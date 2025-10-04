-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Control.Concurrent.STM (TChan)
import Data.Acid (AcidState)
import Data.ByteString
import Data.Dependent.Map (DMap)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Servant (Handler (Handler), ServerError)
import Servant.Links (Link)
import Vira.App.CLI (WebSettings)
import Vira.App.InstanceInfo (InstanceInfo)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.Lib.Logging (runLogActionStdout)
import Vira.Page.ToolsPage.Type (Tool, ToolData)
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

type AppServantStack = (Error ServerError : Reader WebSettings : AppStack)

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
runAppInServant :: AppState -> WebSettings -> Eff AppServantStack a -> Handler a
runAppInServant cfg webSettings =
  Handler . ExceptT . runApp cfg . runReader webSettings . runErrorNoCallStack

-- | Application-wide state available in Effectful stack
data AppState = AppState
  { -- Instance information (hostname, platform)
    instanceInfo :: InstanceInfo
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
  , -- Cached tools data (mutable for refreshing)
    tools :: TVar (DMap Tool ToolData)
  }
