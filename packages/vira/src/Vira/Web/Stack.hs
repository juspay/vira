-- | Web-specific effectful stack extensions
module Vira.Web.Stack where

import Effectful (Eff)
import Effectful.Colog.Simple (tagCurrentThread)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Dynamic (Reader, runReader)
import Servant (Handler (Handler), ServerError)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import Servant.Types.SourceT (SourceT)
import Vira.App.CLI (GlobalSettings (..), WebSettings)
import Vira.App.Stack (AppStack, runApp)
import Vira.App.Type (ViraRuntimeState)
import Vira.Web.Servant (mapSourceT)
import Prelude hiding (Reader, ask, asks, runReader)

type AppServantStack = (Error ServerError : Reader WebSettings : AppStack)

-- | Run the web application stack in Servant 'Handler'
runAppInServant :: GlobalSettings -> ViraRuntimeState -> WebSettings -> Eff AppServantStack a -> Handler a
runAppInServant globalSettings viraRuntimeState webSettings action = do
  Handler . ExceptT . runApp globalSettings viraRuntimeState . runReader webSettings . runErrorNoCallStack $ do
    tagWebThread
    action

runStreamHandler ::
  (MonadIO m) =>
  GlobalSettings ->
  ViraRuntimeState ->
  SourceT (Eff AppStack) a ->
  m (RecommendedEventSourceHeaders (SourceT IO a))
runStreamHandler globalSettings viraRuntimeState h = do
  tagWebThread
  pure $
    recommendedEventSourceHeaders $
      mapSourceT
        (runApp globalSettings viraRuntimeState)
        h

-- Replace ugly thread label for warp handlers with our usual emoji tag.
tagWebThread :: (MonadIO m) => m ()
tagWebThread =
  tagCurrentThread "🌐"
