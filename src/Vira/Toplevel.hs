{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.Toplevel (
  runVira,
) where

import Control.Exception (bracket)
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import Paths_vira qualified
import Servant.Server.Generic (genericServe)
import Vira.App (AppStack, Settings (..))
import Vira.App qualified as App
import Vira.App.CLI qualified as CLI
import Vira.App.LinkTo.Resolve (linkTo)
import Vira.App.Logging
import Vira.Routes qualified as Routes
import Vira.State.Core (closeViraState, openViraState)
import Vira.State.Type qualified as State
import Vira.Supervisor qualified
import Prelude hiding (Reader, ask, runReader)

-- | Run the Vira application
runVira :: IO ()
runVira = do
  Utf8.withUtf8 $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    appIO =<< CLI.parseCLI
  where
    -- Like `app` but in `IO`
    appIO :: Settings -> IO ()
    appIO settings = do
      let repos = settings.repo.cloneUrls <&> repoFromUrl
      bracket (openViraState repos) closeViraState $ \acid -> do
        supervisor <- Vira.Supervisor.newSupervisor
        let st = App.AppState {linkTo = linkTo, ..}
        App.runApp st $ app settings

    -- Vira application for given `Settings`
    app :: (HasCallStack) => Settings -> Eff AppStack ()
    app settings = do
      log Info $ "Launching vira (" <> settings.instanceName <> ") at http://" <> settings.host <> ":" <> show settings.port
      log Debug $ "Settings: " <> show settings
      staticDir <- liftIO Paths_vira.getDataDir
      log Debug $ "Serving static files from: " <> show staticDir
      let staticMiddleware = staticPolicy $ noDots >-> addBase staticDir
      cfg <- ask
      let servantApp = genericServe $ Routes.handlers cfg
      let host = fromString $ toString settings.host
      let warpSettings = Warp.defaultSettings & Warp.setHost host & Warp.setPort settings.port
      liftIO $ Warp.runSettings warpSettings $ staticMiddleware servantApp

-- | Convert a git repository URL to a `State.Repo` record.
repoFromUrl :: Text -> State.Repo
repoFromUrl url =
  State.Repo (nameForGitUrl url) (toText url)

{- | Convert a git repository URL to a name representing it.

For example, `https://github.com/user/foo.git` becomes `foo`.
-}
nameForGitUrl :: Text -> State.RepoName
nameForGitUrl url =
  let
    takeBaseName = T.reverse . T.takeWhile (/= '/') . T.reverse
    name = let s = takeBaseName url in fromMaybe s $ T.stripSuffix ".git" s
   in
    (fromString . toString) name
