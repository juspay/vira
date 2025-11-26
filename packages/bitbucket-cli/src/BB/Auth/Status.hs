{- | Bitbucket authentication status checking

Check if Bitbucket CLI is authenticated by verifying config file exists.
-}
module BB.Auth.Status (
  AuthStatus (..),
  checkAuthStatus,
) where

import BB.Config qualified as Config
import Bitbucket.API.V1.Core (ServerEndpoint)
import Data.Map.Strict qualified as Map

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
