-- | Effectful stack for our app.
module Vira.App.Stack where

import Colog (Message)
import Data.Acid (AcidState)
import Data.Binary.Builder (toLazyByteString)
import Effectful (Eff, IOE, runEff)
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic (Reader, ask, runReader)
import Lucid.Base (Html, HtmlT, ToHtml (toHtmlRaw))
import Lucid.Base qualified as Lucid
import Servant (Handler (Handler), ServerError)
import Servant.Links (Link, URI, linkURI)
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
runAppInServant :: AppState -> Eff (Error ServerError : AppStack) a -> Handler a
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
  }

{- | Like `Html` but can do application effects

Use `lift` to perform effects.
-}
type VHtml = HtmlT (Eff AppServantStack)

-- | Convert a `VHtml` to a Lucid `Html`
runVHtml :: VHtml () -> Eff AppServantStack (Html ())
runVHtml htmlT = do
  (builder, _) <- Lucid.runHtmlT htmlT
  pure $ toHtmlRaw $ toLazyByteString builder

hoistVHtml :: Html () -> VHtml ()
hoistVHtml = Lucid.hoistHtmlT (pure . runIdentity)

-- | Helper to get a Link for a LinkTo (for hxPostSafe_ and similar)
linkToLink :: LinkTo -> VHtml Link
linkToLink linkToValue = do
  cfg <- lift $ ask @AppState
  pure $ linkTo cfg linkToValue

-- | Helper to get a URI for a LinkTo
linkToUri :: LinkTo -> VHtml URI
linkToUri linkToValue = do
  link <- linkToLink linkToValue
  pure $ linkURI link

-- | Helper to get a URL string for a LinkTo (for href attributes)
linkToUrl :: LinkTo -> VHtml Text
linkToUrl linkToValue = do
  uri <- linkToUri linkToValue
  pure $ toText $ show @String $ uri
