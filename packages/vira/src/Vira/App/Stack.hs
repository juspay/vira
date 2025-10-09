{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Control.Concurrent.STM (TChan, TVar)
import Control.Exception (throwIO)
import Data.Acid (AcidState)
import Data.ByteString
import Data.Map (Map)
import Data.Set (Set)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Async, Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Git (RepoName)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, runReader)
import Servant (Handler (Handler), ServerError)
import Servant.Links (Link)
import System.IO.Error (userError)
import Vira.App.CLI (GlobalSettings (..), WebSettings)
import Vira.App.InstanceInfo (InstanceInfo)
import Vira.App.LinkTo.Type (LinkTo)
import Vira.Lib.Logging (runLogActionStdout)
import Vira.Refresh.Type (RefreshConfig, RefreshDaemon, RefreshState, RefreshStatus)
import Vira.State.Core (ViraState)
import Vira.Supervisor.Type (TaskSupervisor)
import Vira.Tool.Type.Tools (Tools)
import Prelude hiding (Reader, ask, asks, runReader)

type AppStack =
  '[ Reader AppState
   , Concurrent
   , Process
   , FileSystem
   , Error Text
   , Log Message
   , IOE
   ]

type AppServantStack = (Error ServerError : Reader WebSettings : AppStack)

-- | Run the application stack in IO monad
runApp :: GlobalSettings -> AppState -> Eff AppStack a -> IO a
runApp globalSettings appState action = do
  result <-
    runEff
      . runLogActionStdout (logLevel globalSettings)
      . runErrorNoCallStack @Text
      . runFileSystem
      . runProcess
      . runConcurrent
      . runReader appState
      $ action
  either (throwIO . userError . toString) pure result

-- | Like `runApp`, but for Servant 'Handler'.
runAppInServant :: GlobalSettings -> AppState -> WebSettings -> Eff AppServantStack a -> Handler a
runAppInServant globalSettings appState webSettings =
  Handler . ExceptT . runApp globalSettings appState . runReader webSettings . runErrorNoCallStack

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
    tools :: TVar Tools
  , -- Refresh daemon with its state
    refreshDaemon :: TVar (Maybe RefreshDaemon)
  }
