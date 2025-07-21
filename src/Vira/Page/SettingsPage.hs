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
import Vira.App.Logging
import Vira.Lib.Attic (AtticServer (..))
import Vira.Lib.HTMX (hxPostSafe_)
import Vira.State.Acid qualified as St
import Vira.State.Type hiding (repoName)
import Vira.Widgets qualified as W
import Prelude hiding (ask, for_)

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _updateCachix :: mode :- "cachix" :> ReqBody '[FormUrlEncoded] CachixSettings :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _updateAttic :: mode :- "attic" :> ReqBody '[FormUrlEncoded] AtticSettings :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _addRepo :: mode :- "repo" :> ReqBody '[FormUrlEncoded] Repo :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  , _removeRepo :: mode :- "repo" :> "remove" :> ReqBody '[FormUrlEncoded] RepoName :> Post '[HTML] (Headers '[HXRefresh] (Html ()))
  }
  deriving stock (Generic)

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

updateCachixHandler :: CachixSettings -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
updateCachixHandler settings = do
  App.update $ St.SetCachixSettingsA settings
  log Info $ "Updated cachix settings for " <> settings.cachixName
  pure $ addHeader True "Ok"

updateAtticHandler :: AtticSettings -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
updateAtticHandler settings = do
  App.update $ St.SetAtticSettingsA settings
  log Info $ "Updated attic settings for " <> settings.atticServer.serverName <> ":" <> toText settings.atticCacheName
  pure $ addHeader True "Ok"

addRepoHandler :: Repo -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
addRepoHandler repo = do
  App.update $ St.AddNewRepoA repo
  log Info $ "Added repository " <> toText repo.name
  pure $ addHeader True "Ok"

removeRepoHandler :: RepoName -> Eff App.AppServantStack (Headers '[HXRefresh] (Html ()))
removeRepoHandler name = do
  App.query (St.GetRepoByNameA name) >>= \case
    Just _repo -> do
      App.update $ St.DeleteRepoByNameA name
      log Info $ "Removed repository " <> toText name
      pure $ addHeader True "Ok"
    Nothing -> do
      log Warning $ "Attempted to remove non-existent repository " <> toText name
      pure $ addHeader True "Not Found"

viewSettings :: (LinkTo.LinkTo -> Link) -> AppSettings -> [Repo] -> Html ()
viewSettings linkTo settings repos =
  div_ [class_ "space-y-8"] $ do
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] repositories
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] $
      cachix settings.cachix
    section_ [class_ "bg-white border-2 border-gray-300 rounded-xl shadow-md p-6"] $
      attic settings.attic
  where
    cachix :: Maybe CachixSettings -> Html ()
    cachix mCachix = do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Cachix"
      form_ [hxPostSafe_ $ linkTo LinkTo.SettingsCachix, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
        textFieldFor "cachixName" "Cache Name" (maybe "" (.cachixName) mCachix)
        textFieldFor "authToken" "Auth Token" "" -- Don't display existing token
        W.viraButton_ [type_ "submit"] "Save"

    attic :: Maybe AtticSettings -> Html ()
    attic mAttic = do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Attic"
      form_ [hxPostSafe_ $ linkTo LinkTo.SettingsAttic, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
        textFieldFor
          "serverName"
          "Server Name"
          (maybe "" ((\(AtticServer sn _) -> sn) . (.atticServer)) mAttic)
        textFieldFor
          "serverUrl"
          "Server URL"
          (maybe "" ((\(AtticServer _ su) -> su) . (.atticServer)) mAttic)
        textFieldFor
          "atticCacheName"
          "Cache Name"
          (maybe "" (toText . (.atticCacheName)) mAttic)
        textFieldFor "atticToken" "Token" ""
        W.viraButton_ [type_ "submit"] "Save"

    repositories :: Html ()
    repositories = do
      h2_ [class_ "text-2xl font-semibold mb-4 text-gray-800"] "Repositories"

      div_ [class_ "mb-6"] $ do
        h3_ [class_ "text-lg font-medium mb-3"] "New Repository"
        form_ [hxPostSafe_ $ linkTo LinkTo.SettingsAddRepo, hxSwapS_ InnerHTML, class_ "space-y-4"] $ do
          textFieldFor "name" "Name" ""
          textFieldFor "cloneUrl" "Clone URL" ""
          details_ [class_ "border border-gray-300 rounded-md p-4"] $ do
            summary_ [class_ "cursor-pointer text-lg font-medium mb-2"] "Settings"
            textFieldFor "dummy" "Dummy" ""
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
                  input_ [type_ "hidden", name_ "unRepoName", value_ $ toText $ toString repo.name]
                  W.viraButton_ [type_ "submit", class_ "bg-red-600 hover:bg-red-700"] "Remove"

    -- TODO(CRUD): type-level guarantees for `fieldName`
    textFieldFor :: Text -> Text -> Text -> Html ()
    textFieldFor fieldName label value = div_ $ do
      _ <- label_ [for_ fieldName, class_ "block text-sm font-medium text-gray-700"] $ toHtml label
      input_
        [ type_ "text"
        , id_ fieldName
        , name_ fieldName
        , class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"
        , value_ value
        ]
