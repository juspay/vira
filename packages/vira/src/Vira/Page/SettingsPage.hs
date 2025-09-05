{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Vira's global settings page
module Vira.Page.SettingsPage (
  Routes (..),
  handlers,
)
where

import Colog (Severity (..))
import Effectful (Eff)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Servant.Response
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Lucid.Htmx.Contrib (hxConfirm_, hxPostSafe_)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Attic (AtticServer (..))
import Vira.Lib.Logging
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings (..), CachixSettings (..), RemoteBuilder (..), RemoteBuilderForm (..), RemoteBuilderId)
import Vira.Widgets.Alert qualified as W
import Vira.Widgets.Button qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Form qualified as W
import Vira.Widgets.Layout qualified as W
import Prelude hiding (ask, for_)

type FormReq a = ReqBody '[FormUrlEncoded] a
type FormResp = Headers '[HXRefresh] (Html ())

data Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  , _updateCachix :: mode :- "cachix" :> FormReq CachixSettings :> Post '[HTML] FormResp
  , _deleteCachix :: mode :- "cachix" :> "delete" :> Post '[HTML] FormResp
  , _updateAttic :: mode :- "attic" :> FormReq AtticSettings :> Post '[HTML] FormResp
  , _deleteAttic :: mode :- "attic" :> "delete" :> Post '[HTML] FormResp
  , _addRemoteBuilder :: mode :- "remote-builder" :> FormReq RemoteBuilderForm :> Post '[HTML] FormResp
  , _updateRemoteBuilder :: mode :- "remote-builder" :> Capture "id" RemoteBuilderId :> FormReq RemoteBuilderForm :> Post '[HTML] FormResp
  , _deleteRemoteBuilder :: mode :- "remote-builder" :> Capture "id" RemoteBuilderId :> "delete" :> Post '[HTML] FormResp
  }
  deriving stock (Generic)

handlers :: App.AppState -> Routes AsServer
handlers cfg =
  Routes
    { _view = App.runAppInServant cfg . App.runAppHtml $ viewHandler
    , _updateCachix = App.runAppInServant cfg . updateCachixHandler
    , _deleteCachix = App.runAppInServant cfg deleteCachixHandler
    , _updateAttic = App.runAppInServant cfg . updateAtticHandler
    , _deleteAttic = App.runAppInServant cfg deleteAtticHandler
    , _addRemoteBuilder = App.runAppInServant cfg . addRemoteBuilderHandler
    , _updateRemoteBuilder = \builderId -> App.runAppInServant cfg . updateRemoteBuilderHandler builderId
    , _deleteRemoteBuilder = App.runAppInServant cfg . deleteRemoteBuilderHandler
    }

viewHandler :: AppHtml ()
viewHandler = do
  W.layout [LinkTo.Settings] viewSettings

updateCachixHandler :: CachixSettings -> Eff App.AppServantStack FormResp
updateCachixHandler settings = do
  App.update $ St.SetCachixSettingsA (Just settings)
  log Info $ "Updated cachix settings for " <> settings.cachixName
  pure $ addHeader True "Ok"

deleteCachixHandler :: Eff App.AppServantStack FormResp
deleteCachixHandler = do
  App.update $ St.SetCachixSettingsA Nothing
  log Info "Deleted cachix settings"
  pure $ addHeader True "Ok"

updateAtticHandler :: AtticSettings -> Eff App.AppServantStack FormResp
updateAtticHandler settings = do
  App.update $ St.SetAtticSettingsA (Just settings)
  log Info $ "Updated attic settings for " <> settings.atticServer.serverName <> ":" <> toText settings.atticCacheName
  pure $ addHeader True "Ok"

deleteAtticHandler :: Eff App.AppServantStack FormResp
deleteAtticHandler = do
  App.update $ St.SetAtticSettingsA Nothing
  log Info "Deleted attic settings"
  pure $ addHeader True "Ok"

addRemoteBuilderHandler :: RemoteBuilderForm -> Eff App.AppServantStack FormResp
addRemoteBuilderHandler form = do
  _builder <-
    App.update $
      St.AddRemoteBuilderA
        form.remoteBuilderFormUser
        form.remoteBuilderFormHost
        form.remoteBuilderFormPlatforms
        form.remoteBuilderFormNote
  log Info $ "Added remote builder: " <> form.remoteBuilderFormUser <> "@" <> form.remoteBuilderFormHost
  pure $ addHeader True "Ok"

updateRemoteBuilderHandler :: RemoteBuilderId -> RemoteBuilderForm -> Eff App.AppServantStack FormResp
updateRemoteBuilderHandler builderId form = do
  App.update $
    St.UpdateRemoteBuilderA
      builderId
      form.remoteBuilderFormUser
      form.remoteBuilderFormHost
      form.remoteBuilderFormPlatforms
      form.remoteBuilderFormNote
  log Info $ "Updated remote builder " <> show builderId <> ": " <> form.remoteBuilderFormUser <> "@" <> form.remoteBuilderFormHost
  pure $ addHeader True "Ok"

deleteRemoteBuilderHandler :: RemoteBuilderId -> Eff App.AppServantStack FormResp
deleteRemoteBuilderHandler builderId = do
  App.update $ St.DeleteRemoteBuilderA builderId
  log Info $ "Deleted remote builder " <> show builderId
  pure $ addHeader True "Ok"

viewSettings :: App.AppHtml ()
viewSettings = do
  mCachix <- lift $ App.query St.GetCachixSettingsA
  mAttic <- lift $ App.query St.GetAtticSettingsA
  remoteBuilders <- lift $ App.query St.GetAllRemoteBuildersA
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Settings" $ do
      p_ [class_ "text-gray-600"] "Configure build cache providers and CI/CD integrations"

    div_ [class_ "grid gap-8"] $ do
      -- Cachix Configuration
      W.viraCard_ [class_ "p-6"] $ do
        div_ [class_ "flex items-center mb-6"] $ do
          span_ [class_ "h-8 w-8 mr-3 bg-blue-100 rounded-lg flex items-center justify-center text-blue-600 font-bold"] "C"
          h3_ [class_ "text-xl font-bold text-gray-900"] "Cachix Configuration"

        case mCachix of
          Just cachix -> do
            W.viraAlert_ W.AlertSuccess $ do
              p_ [class_ "text-green-800"] $ do
                "Connected to cache: "
                strong_ $ toHtml cachix.cachixName
            W.viraDivider_
          Nothing -> do
            W.viraAlert_ W.AlertInfo $ do
              p_ [class_ "text-blue-800"] "Configure Cachix to enable build caching"
            W.viraDivider_

        cachixForm mCachix

      -- Attic Configuration
      W.viraCard_ [class_ "p-6"] $ do
        div_ [class_ "flex items-center mb-6"] $ do
          span_ [class_ "h-8 w-8 mr-3 bg-purple-100 rounded-lg flex items-center justify-center text-purple-600 font-bold"] "A"
          h3_ [class_ "text-xl font-bold text-gray-900"] "Attic Configuration"

        case mAttic of
          Just attic -> do
            W.viraAlert_ W.AlertSuccess $ do
              p_ [class_ "text-green-800"] $ do
                "Connected to "
                strong_ $ toHtml attic.atticServer.serverName
                " ("
                toHtml $ toText attic.atticCacheName
                ")"
            W.viraDivider_
          Nothing -> do
            W.viraAlert_ W.AlertInfo $ do
              p_ [class_ "text-blue-800"] "Configure Attic for distributed build caching"
            W.viraDivider_

        atticForm mAttic

      -- Remote Builders Configuration
      W.viraCard_ [class_ "p-6"] $ do
        div_ [class_ "flex items-center mb-6"] $ do
          span_ [class_ "h-8 w-8 mr-3 bg-green-100 rounded-lg flex items-center justify-center text-green-600 font-bold"] "R"
          h3_ [class_ "text-xl font-bold text-gray-900"] "Remote Builders"

        if null remoteBuilders
          then do
            W.viraAlert_ W.AlertInfo $ do
              p_ [class_ "text-blue-800"] "Add remote builders for distributed builds across platforms"
            W.viraDivider_
          else do
            W.viraAlert_ W.AlertSuccess $ do
              p_ [class_ "text-green-800"] $ do
                "Configured "
                strong_ $ toHtml (show (length remoteBuilders) :: Text)
                " remote builder(s)"
            W.viraDivider_

            -- List existing builders
            div_ [class_ "space-y-4 mb-6"] $ do
              forM_ remoteBuilders $ \builder -> do
                div_ [class_ "border border-gray-200 rounded-lg p-4"] $ do
                  div_ [class_ "flex justify-between items-start"] $ do
                    div_ $ do
                      div_ [class_ "font-medium text-gray-900"] $ do
                        toHtml builder.remoteBuilderUser <> "@" <> toHtml builder.remoteBuilderHost
                      div_ [class_ "text-sm text-gray-500 mt-1"] $ do
                        "Platforms: " <> toHtml (toText $ intercalate ", " (map show builder.remoteBuilderPlatforms))
                      whenJust builder.remoteBuilderNote $ \note -> do
                        div_ [class_ "text-sm text-gray-600 mt-1"] $ toHtml note
                    div_ [class_ "flex gap-2"] $ do
                      deleteBuilderLink <- lift $ App.getLink $ LinkTo.SettingsDeleteRemoteBuilder builder.remoteBuilderId
                      form_ [hxPostSafe_ deleteBuilderLink, hxSwapS_ InnerHTML, hxConfirm_ "Are you sure you want to delete this remote builder?"] $ do
                        W.viraButton_ W.ButtonDestructive [type_ "submit", class_ "px-3 py-1 text-xs"] "Delete"
            W.viraDivider_

        remoteBuilderForm
  where
    cachixForm :: Maybe CachixSettings -> App.AppHtml ()
    cachixForm mCachixSettings = do
      updateCachixLink <- lift $ App.getLink LinkTo.SettingsUpdateCachix
      form_ [id_ "cachix-update", hxPostSafe_ updateCachixLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
        W.viraFormGroup_
          ( withFieldName @CachixSettings @"cachixName" $ \name ->
              W.viraLabel_ [for_ name] "Cache Name"
          )
          ( withFieldName @CachixSettings @"cachixName" $ \name ->
              W.viraInput_
                [ type_ "text"
                , name_ name
                , value_ $ maybe "" (.cachixName) mCachixSettings
                , placeholder_ "my-cache"
                ]
          )

        W.viraFormGroup_
          ( withFieldName @CachixSettings @"authToken" $ \name ->
              W.viraLabel_ [for_ name] "Auth Token"
          )
          ( withFieldName @CachixSettings @"authToken" $ \name ->
              W.viraInput_ $
                [ type_ "password"
                , name_ name
                , placeholder_ $ maybe "Enter your Cachix auth token" (const "••••••••••••") mCachixSettings
                ]
                  <> maybe [] (const []) mCachixSettings
          )

        div_ [class_ "flex items-center gap-3 pt-4"] $ do
          W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "cachix-update"] $ do
            case mCachixSettings of
              Nothing -> "Connect Cachix"
              Just _ -> "Update Settings"

      -- Disconnect form outside the main form to avoid nesting
      whenJust mCachixSettings $ \_ -> do
        deleteCachixLink <- lift $ App.getLink LinkTo.SettingsDeleteCachix
        form_ [hxPostSafe_ deleteCachixLink, hxSwapS_ InnerHTML, hxConfirm_ "Are you sure you want to disconnect Cachix? This action cannot be undone.", class_ "mt-3"] $ do
          W.viraButton_ W.ButtonDestructive [type_ "submit"] "Disconnect"

    atticForm :: Maybe AtticSettings -> App.AppHtml ()
    atticForm mAtticSettings = do
      updateAtticLink <- lift $ App.getLink LinkTo.SettingsUpdateAttic
      form_ [id_ "attic-update", hxPostSafe_ updateAtticLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
        div_ [class_ "grid gap-4 lg:grid-cols-2"] $ do
          W.viraFormGroup_
            ( withFieldName @AtticServer @"serverName" $ \name ->
                W.viraLabel_ [for_ name] "Server Name"
            )
            ( withFieldName @AtticServer @"serverName" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , value_ $ maybe "" ((.serverName) . (.atticServer)) mAtticSettings
                  , placeholder_ "my-attic"
                  ]
            )

          W.viraFormGroup_
            ( withFieldName @AtticServer @"serverUrl" $ \name ->
                W.viraLabel_ [for_ name] "Server URL"
            )
            ( withFieldName @AtticServer @"serverUrl" $ \name ->
                W.viraInput_
                  [ type_ "url"
                  , name_ name
                  , value_ $ maybe "" ((.serverUrl) . (.atticServer)) mAtticSettings
                  , placeholder_ "https://attic.example.com"
                  ]
            )

        div_ [class_ "grid gap-4 lg:grid-cols-2"] $ do
          W.viraFormGroup_
            ( withFieldName @AtticSettings @"atticCacheName" $ \name ->
                W.viraLabel_ [for_ name] "Cache Name"
            )
            ( withFieldName @AtticSettings @"atticCacheName" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , value_ $ maybe "" (toText . (.atticCacheName)) mAtticSettings
                  , placeholder_ "cache-name"
                  ]
            )

          W.viraFormGroup_
            ( withFieldName @AtticSettings @"atticToken" $ \name ->
                W.viraLabel_ [for_ name] "Token"
            )
            ( withFieldName @AtticSettings @"atticToken" $ \name ->
                W.viraInput_ $
                  [ type_ "password"
                  , name_ name
                  , placeholder_ $ maybe "Enter your Attic token" (const "••••••••••••") mAtticSettings
                  ]
                    <> maybe [] (const []) mAtticSettings
            )

        div_ [class_ "flex items-center gap-3 pt-4"] $ do
          W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "attic-update"] $ do
            case mAtticSettings of
              Nothing -> "Connect Attic"
              Just _ -> "Update Settings"

      -- Disconnect form outside the main form to avoid nesting
      whenJust mAtticSettings $ \_ -> do
        deleteAtticLink <- lift $ App.getLink LinkTo.SettingsDeleteAttic
        form_ [hxPostSafe_ deleteAtticLink, hxSwapS_ InnerHTML, hxConfirm_ "Are you sure you want to disconnect Attic? This action cannot be undone.", class_ "mt-3"] $ do
          W.viraButton_ W.ButtonDestructive [type_ "submit"] "Disconnect"

    remoteBuilderForm :: App.AppHtml ()
    remoteBuilderForm = do
      addBuilderLink <- lift $ App.getLink LinkTo.SettingsAddRemoteBuilder
      form_ [id_ "remote-builder-add", hxPostSafe_ addBuilderLink, hxSwapS_ InnerHTML, class_ "space-y-6"] $ do
        div_ [class_ "grid gap-4 lg:grid-cols-2"] $ do
          W.viraFormGroup_
            ( withFieldName @RemoteBuilderForm @"remoteBuilderFormUser" $ \name ->
                W.viraLabel_ [for_ name] "SSH Username"
            )
            ( withFieldName @RemoteBuilderForm @"remoteBuilderFormUser" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , placeholder_ "username"
                  , required_ "true"
                  ]
            )

          W.viraFormGroup_
            ( withFieldName @RemoteBuilderForm @"remoteBuilderFormHost" $ \name ->
                W.viraLabel_ [for_ name] "Host"
            )
            ( withFieldName @RemoteBuilderForm @"remoteBuilderFormHost" $ \name ->
                W.viraInput_
                  [ type_ "text"
                  , name_ name
                  , placeholder_ "builder.example.com"
                  , required_ "true"
                  ]
            )

        W.viraFormGroup_
          ( withFieldName @RemoteBuilderForm @"remoteBuilderFormPlatforms" $ \name ->
              W.viraLabel_ [for_ name] "Supported Platforms"
          )
          ( withFieldName @RemoteBuilderForm @"remoteBuilderFormPlatforms" $ \name -> do
              div_ [class_ "space-y-2"] $ do
                div_ [class_ "flex items-center"] $ do
                  input_ [type_ "checkbox", name_ name, value_ "linux", id_ "platform-linux", class_ "rounded border-gray-300"]
                  label_ [for_ "platform-linux", class_ "ml-2 text-sm text-gray-700"] "Linux (x86_64)"
                div_ [class_ "flex items-center"] $ do
                  input_ [type_ "checkbox", name_ name, value_ "macos", id_ "platform-macos", class_ "rounded border-gray-300"]
                  label_ [for_ "platform-macos", class_ "ml-2 text-sm text-gray-700"] "macOS (ARM64)"
                div_ [class_ "flex items-center"] $ do
                  input_ [type_ "checkbox", name_ name, value_ "macos-intel", id_ "platform-macos-intel", class_ "rounded border-gray-300"]
                  label_ [for_ "platform-macos-intel", class_ "ml-2 text-sm text-gray-700"] "macOS (Intel)"
          )

        W.viraFormGroup_
          ( withFieldName @RemoteBuilderForm @"remoteBuilderFormNote" $ \name ->
              W.viraLabel_ [for_ name] "Description (Optional)"
          )
          ( withFieldName @RemoteBuilderForm @"remoteBuilderFormNote" $ \name ->
              W.viraInput_
                [ type_ "text"
                , name_ name
                , placeholder_ "Optional description or notes"
                ]
          )

        div_ [class_ "flex items-center gap-3 pt-4"] $ do
          W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "remote-builder-add"] "Add Remote Builder"

withFieldName ::
  forall record field a r.
  (KnownSymbol field, HasField field record a) =>
  (Text -> r) ->
  r
withFieldName k =
  let fieldName = toText (symbolVal (Proxy @field))
   in k fieldName
