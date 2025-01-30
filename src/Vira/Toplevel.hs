{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | Top-level routes and views
module Vira.Toplevel (
  runVira,
) where

import Control.Exception (bracket)
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static (
  addBase,
  noDots,
  staticPolicy,
  (>->),
 )
import OptEnvConf qualified
import Paths_vira (version)
import Servant.API (Get, NamedRoutes, (:>))
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Generic (GenericMode (type (:-)))
import Servant.Links (Link, fieldLink, linkURI)
import Servant.Server.Generic (AsServer, genericServe)
import Vira.App (AppStack, Settings (..), (//), (/:))
import Vira.App qualified as App
import Vira.App.LinkTo (LinkTo (..))
import Vira.App.Logging
import Vira.Page.JobPage qualified as JobPage
import Vira.Page.RegistryPage qualified as RegistryPage
import Vira.Page.RepoPage qualified as RepoPage
import Vira.State.Core (closeViraState, openViraState)
import Vira.State.Type qualified as State
import Vira.Supervisor qualified
import Vira.Widgets qualified as W
import Prelude hiding (Reader, ask, runReader)

data Routes mode = Routes
  { _home :: mode :- Get '[HTML] (Html ())
  , _repos :: mode :- "r" Servant.API.:> NamedRoutes RegistryPage.Routes
  , _jobs :: mode :- "j" Servant.API.:> NamedRoutes JobPage.Routes
  , _about :: mode :- "about" Servant.API.:> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

-- | Top-level handlers
handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _home = App.runAppInServant cfg $ do
        pure $ W.layout cfg.linkTo "Welcome" [] $ do
          nav_ [class_ "space-y-2"] $ do
            forM_ menu $ \(name, url) -> do
              a_ [href_ url, class_ "flex items-center p-3 space-x-3 text-blue-700 font-bold transition-colors rounded-md hover:bg-gray-100"] $ do
                name
    , _repos = RegistryPage.handlers cfg
    , _jobs = JobPage.handlers cfg
    , _about = do
        pure $ W.layout cfg.linkTo "About Vira" [About] $ do
          div_ $ do
            a_ [href_ "https://github.com/juspay/vira"] "GitHub Repo"
    }
  where
    linkText = show . linkURI
    menu :: [(Html (), Text)]
    menu =
      [ ("Repos", linkText $ fieldLink _repos // RegistryPage._listing)
      , ("About", linkText $ fieldLink _about)
      ]

-- | Run the Vira application
runVira :: IO ()
runVira = do
  Utf8.withUtf8 $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    settings :: Settings <- OptEnvConf.runSettingsParser version "Nix CI & Cache for teams"
    appIO settings
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
      log Info $ "Launching vira at http://localhost:" <> show settings.port
      log Debug $ "Settings: " <> show settings
      let staticMiddleware = staticPolicy $ noDots >-> addBase "static"
      cfg <- ask
      let servantApp = genericServe $ handlers cfg
      liftIO $ Warp.run settings.port $ staticMiddleware servantApp

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

-- | Define application link hints (`LinkTo`) here.
linkTo :: LinkTo -> Link
linkTo = \case
  Home -> fieldLink _home
  About -> fieldLink _about
  RepoListing -> fieldLink _repos // RegistryPage._listing
  Repo name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._view
  RepoUpdate name -> fieldLink _repos // RegistryPage._repo /: name // RepoPage._update
  Build repo branch -> fieldLink _jobs // JobPage._build /: repo /: branch
  Job jobId -> fieldLink _jobs // JobPage._view /: jobId
