-- | Bitbucket authentication login command
module BB.Command.Auth.Login (
  runLogin,
) where

import BB.Config (ServerConfig (..), getConfigPath)
import BB.Config qualified as Config
import Bitbucket.API.V1.Core (ServerEndpoint (..), Token (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Text.URI (Authority (..), mkURI, unRText)
import Text.URI qualified as URI

-- | Run login command
runLogin :: (Error Text :> es, IOE :> es) => Text -> Eff es ()
runLogin baseUrlText = do
  -- Prompt for token
  liftIO $ putStr "Enter your Bitbucket access token: "
  liftIO $ hFlush stdout
  tokenInput <- Text.strip . toText <$> liftIO getLine

  -- Parse URL to extract host
  -- Normalize URL: prepend https:// if no protocol specified
  let normalizedUrl =
        if "http://" `isPrefixOf` toString baseUrlText || "https://" `isPrefixOf` toString baseUrlText
          then baseUrlText
          else "https://" <> baseUrlText

  uri <- case mkURI normalizedUrl of
    Left parseErr ->
      throwError $ "✗ URL parse error for " <> baseUrlText <> ": " <> show parseErr
    Right u -> pure u

  -- Extract host from URI
  host <- case URI.uriAuthority uri of
    Right auth -> pure $ unRText (authHost auth)
    Left _ ->
      throwError $ "✗ Could not extract host from URL: " <> normalizedUrl

  -- Load existing config or start fresh
  existingServers <- liftIO $ whenRightM Map.empty Config.loadConfig pure

  -- Insert/update server
  let endpoint = ServerEndpoint host
      serverConfig = ServerConfig {token = Token tokenInput}
      updatedServers = Map.insert endpoint serverConfig existingServers

  -- Save config
  liftIO $ Config.saveConfig updatedServers

  configPath <- liftIO getConfigPath
  liftIO $ putTextLn $ "✓ Configuration saved to " <> toText configPath
