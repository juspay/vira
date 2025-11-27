{-# LANGUAGE OverloadedRecordDot #-}

{- | Bitbucket authentication status checking

Check if Bitbucket CLI is authenticated by verifying config file exists.
-}
module BB.Command.Auth.Status (
  AuthStatus (..),
  checkAuthStatus,
  runStatus,
) where

import BB.Config (ConfigError (..), ServerConfig (..), getConfigPath)
import BB.Config qualified as Config
import Bitbucket.API.V1.Core (ServerEndpoint)
import Colog.Message (RichMessage)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified as ER

-- | Authentication status for Bitbucket CLI
data AuthStatus
  = Authenticated
      { servers :: Map ServerEndpoint Config.ServerConfig
      -- ^ All configured servers
      }
  | NotAuthenticated
  deriving stock (Show, Eq)

{- | Check if Bitbucket CLI is authenticated

Checks if the config file exists and has at least one server configured.
-}
checkAuthStatus :: IO AuthStatus
checkAuthStatus = do
  result <- Config.loadConfig
  pure $ case result of
    Right servers
      | not (Map.null servers) -> Authenticated {servers}
    _ -> NotAuthenticated

-- | Run status command
runStatus :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es) => Bool -> Eff es ()
runStatus jsonOutput = do
  configPath <- liftIO getConfigPath
  liftIO Config.loadConfig >>= \case
    Left err -> do
      if jsonOutput
        then do
          let json = object ["authenticated" .= False, "error" .= showConfigError err]
          liftIO $ putLBSLn $ Aeson.encode json
          throwError @Text "Config error"
        else do
          liftIO $ putTextLn $ "✗ " <> showConfigError err
          throwError @Text "Config error"
    Right servers -> do
      if Map.null servers
        then do
          if jsonOutput
            then do
              let json = object ["authenticated" .= False, "error" .= ("No servers configured" :: Text)]
              liftIO $ putLBSLn $ Aeson.encode json
              throwError @Text "No servers configured"
            else do
              liftIO $ putTextLn "✗ No servers configured"
              throwError @Text "No servers configured"
        else do
          -- Test first server (could be enhanced to test all or prompt user)
          case viaNonEmpty head (Map.toList servers) of
            Nothing -> do
              if jsonOutput
                then do
                  let json = object ["authenticated" .= False, "error" .= ("No servers configured" :: Text)]
                  liftIO $ putLBSLn $ Aeson.encode json
                  throwError @Text "No servers configured"
                else do
                  liftIO $ putTextLn "✗ No servers configured"
                  throwError @Text "No servers configured"
            Just (endpoint, serverConfig) ->
              Config.testConnection endpoint serverConfig.token >>= \case
                Left testErr -> do
                  if jsonOutput
                    then do
                      let json =
                            object
                              [ "authenticated" .= False
                              , "config_found" .= True
                              , "connection_error" .= testErr
                              ]
                      liftIO $ putLBSLn $ Aeson.encode json
                      throwError @Text testErr
                    else do
                      liftIO $ putTextLn $ "✗ Connection test failed: " <> testErr
                      throwError @Text testErr
                Right () -> do
                  if jsonOutput
                    then do
                      let json =
                            object
                              [ "authenticated" .= True
                              , "config_path" .= toText configPath
                              , "server_count" .= Map.size servers
                              ]
                      liftIO $ putLBSLn $ Aeson.encode json
                    else do
                      liftIO $ putTextLn "✓ Authenticated"
                      liftIO $ putTextLn $ "  Config: " <> toText configPath
                      liftIO $ putTextLn $ "  Servers: " <> show (Map.size servers)

-- | Show config error
showConfigError :: ConfigError -> Text
showConfigError = \case
  ConfigFileNotFound path -> "Config file not found: " <> toText path
  JsonDecodeError msg -> "JSON decode error: " <> msg
