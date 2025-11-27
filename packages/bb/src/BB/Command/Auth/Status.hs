{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Bitbucket authentication status checking

Check if Bitbucket CLI is authenticated by verifying config file exists.
-}
module BB.Command.Auth.Status (
  checkAuthStatus,
  runStatus,
) where

import BB.Config qualified as Config
import Bitbucket.API.V1.Core (ServerEndpoint (..))
import Colog.Message (RichMessage)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified as ER

{- | Get configured Bitbucket servers

Returns map of configured servers from @~/.config/bb/config.json@.
Returns empty map if no config exists. Throws on malformed JSON.
-}
checkAuthStatus :: IO (Map ServerEndpoint Config.ServerConfig)
checkAuthStatus = do
  Config.loadConfig >>= \case
    Left err -> fail $ toString $ "Failed to load config: " <> err
    Right servers -> pure servers

{- | List configured Bitbucket servers

Returns configured servers from @~/.config/bb/config.json@.
Returns empty if no config file exists. Fails if config file is malformed.

JSON mode outputs @Map ServerEndpoint ServerConfig@.
Human mode prints server hostnames line by line.
-}
runStatus :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es) => Bool -> Eff es ()
runStatus jsonOutput = do
  liftIO Config.loadConfig >>= \case
    Left err -> do
      throwError @Text $ "Failed to decode config: " <> err
    Right servers -> do
      if jsonOutput
        then liftIO $ putLBSLn $ Aeson.encode servers
        else do
          forM_ (Map.keys servers) $ \endpoint -> do
            liftIO $ putTextLn $ "  " <> endpoint.host
