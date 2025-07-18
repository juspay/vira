{-# LANGUAGE OverloadedRecordDot #-}

module Vira.Page.SettingsPage (
  Routes (..),
  handlers,
)
where

import Effectful (Eff)
import Effectful.Reader.Dynamic (ask)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Attic (AtticServer (..))
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.State.Acid qualified as St
import Vira.State.Type hiding (repoName)
import Vira.Widgets qualified as W

import Web.FormUrlEncoded (FromForm)
import Prelude hiding (ask, for_)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _updateCachix :: mode :- "cachix" :> ReqBody '[FormUrlEncoded] CachixSettings :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _updateAttic :: mode :- "attic" :> ReqBody '[FormUrlEncoded] AtticSettings :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _addRepo :: mode :- "repo" :> ReqBody '[FormUrlEncoded] RepoForm :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _removeRepo :: mode :- "repo" :> "remove" :> ReqBody '[FormUrlEncoded] RemoveRepoForm :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  }
  deriving stock (Generic)

data RepoForm = RepoForm
  { repoName :: Text
  , repoCloneUrl :: Text
  }
  deriving stock (Generic, Show)

instance FromForm RepoForm

newtype RemoveRepoForm = RemoveRepoForm
  { removeRepoName :: Text
  }
  deriving stock (Generic, Show)

instance FromForm RemoveRepoForm

handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _view = App.runAppInServant cfg viewHandler
    , _updateCachix = App.runAppInServant cfg . updateCachixHandler
    , _updateAttic = App.runAppInServant cfg . updateAtticHandler
    , _addRepo = App.runAppInServant cfg . addRepoHandler
    , _removeRepo = App.runAppInServant cfg . removeRepoHandler
    }

viewHandler :: Eff App.AppServantStack (Html ())
viewHandler = do
  cfg <- ask
  settings <- App.query St.GetAppSettingsA
  repos <- App.query St.GetAllReposA
  pure $ W.layout cfg [LinkTo.Settings] $ viewSettings cfg.linkTo settings repos

viewSettings :: (LinkTo.LinkTo -> Link) -> AppSettings -> [Repo] -> Html ()
viewSettings linkTo settings repos =
  div_ [class_ "space-y-8"] $ do
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] $
      repoManagement linkTo repos
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] $
      cachixForm linkTo settings.cachix
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] $
      atticForm linkTo settings.attic

cachixForm :: (LinkTo.LinkTo -> Link) -> Maybe CachixSettings -> Html ()
cachixForm linkTo mCachix = do
  h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Cachix"
  form_ [hxPostSafe_ $ linkTo LinkTo.SettingsCachix, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
    div_ $ do
      label_ [for_ "cachixName", class_ "block text-sm font-medium text-gray-700"] "Cache Name"
      input_
        [ type_ "text"
        , name_ "cachixName"
        , id_ "cachixName"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" (.cachixName) mCachix
        ]
    div_ $ do
      label_ [for_ "authToken", class_ "block text-sm font-medium text-gray-700"] "Auth Token"
      input_
        [ type_ "password"
        , name_ "authToken"
        , id_ "authToken"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" (.authToken) mCachix
        ]
    W.viraButton_ [type_ "submit"] "Save"

atticForm :: (LinkTo.LinkTo -> Link) -> Maybe AtticSettings -> Html ()
atticForm linkTo mAttic = do
  h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Attic"
  form_ [hxPostSafe_ $ linkTo LinkTo.SettingsAttic, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
    div_ $ do
      label_ [for_ "serverName", class_ "block text-sm font-medium text-gray-700"] "Server Name"
      input_
        [ type_ "text"
        , name_ "serverName"
        , id_ "serverName"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" ((\(AtticServer sn _) -> sn) . (.atticServer)) mAttic
        ]
    div_ $ do
      label_ [for_ "serverUrl", class_ "block text-sm font-medium text-gray-700"] "Server URL"
      input_
        [ type_ "url"
        , name_ "serverUrl"
        , id_ "serverUrl"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" ((\(AtticServer _ su) -> su) . (.atticServer)) mAttic
        ]
    div_ $ do
      label_ [for_ "atticCacheName", class_ "block text-sm font-medium text-gray-700"] "Cache Name"
      input_
        [ type_ "text"
        , name_ "atticCacheName"
        , id_ "atticCacheName"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" (toText . toString . (.atticCacheName)) mAttic
        ]
    div_ $ do
      label_ [for_ "atticToken", class_ "block text-sm font-medium text-gray-700"] "Token"
      input_
        [ type_ "password"
        , name_ "atticToken"
        , id_ "atticToken"
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ $ maybe "" (toText . toString . (.atticToken)) mAttic
        ]
    W.viraButton_ [type_ "submit"] "Save"

repoManagement :: (LinkTo.LinkTo -> Link) -> [Repo] -> Html ()
repoManagement linkTo repos = do
  h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Repositories"

  div_ [class_ "mb-6"] $ do
    h3_ [class_ "text-lg font-medium mb-3"] "Add New Repository"
    form_ [hxPostSafe_ $ linkTo LinkTo.SettingsAddRepo, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
      div_ $ do
        label_ [for_ "repoName", class_ "block text-sm font-medium text-gray-700"] "Repository Name"
        input_
          [ type_ "text"
          , name_ "repoName"
          , id_ "repoName"
          , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
          , placeholder_ "my-repo"
          ]
      div_ $ do
        label_ [for_ "repoCloneUrl", class_ "block text-sm font-medium text-gray-700"] "Clone URL"
        input_
          [ type_ "url"
          , name_ "repoCloneUrl"
          , id_ "repoCloneUrl"
          , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
          , placeholder_ "https://github.com/user/repo.git"
          ]
      W.viraButton_ [type_ "submit"] "Add"

  div_ $ do
    h3_ [class_ "text-lg font-medium mb-3"] "Existing Repositories"
    if null repos
      then p_ [class_ "text-gray-500 italic"] "No repositories configured"
      else div_ [class_ "space-y-2"] $
        forM_ repos $ \repo ->
          div_ [class_ "flex items-center justify-between p-3 bg-gray-50 rounded-md"] $ do
            div_ $ do
              strong_ [class_ "text-gray-900"] $ toHtml $ toString repo.name
              br_ []
              span_ [class_ "text-sm text-gray-600"] $ toHtml repo.cloneUrl
            form_ [hxPostSafe_ $ linkTo LinkTo.SettingsRemoveRepo, hxSwapS_ InnerHTML, class_ "inline"] $ do
              input_ [type_ "hidden", name_ "removeRepoName", value_ $ toText $ toString repo.name]
              W.viraButton_ [type_ "submit", class_ "bg-red-600 hover:bg-red-700"] "Remove"

updateCachixHandler :: CachixSettings -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
updateCachixHandler settings = do
  App.update $ St.SetCachixSettingsA settings
  pure $ addHeader True "Ok"

updateAtticHandler :: AtticSettings -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
updateAtticHandler settings = do
  App.update $ St.SetAtticSettingsA settings
  pure $ addHeader True "Ok"

addRepoHandler :: RepoForm -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
addRepoHandler form = do
  let repo =
        Repo
          { name = RepoName form.repoName
          , cloneUrl = form.repoCloneUrl
          , settings = RepoSettings ()
          }
  App.update $ St.AddNewRepoA repo
  pure $ addHeader True "Ok"

removeRepoHandler :: RemoveRepoForm -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
removeRepoHandler form = do
  let repoName = RepoName form.removeRepoName
  App.query (St.GetRepoByNameA repoName) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA repoName
      pure $ addHeader True "Ok"
    Nothing ->
      pure $ addHeader True "Not Found"
