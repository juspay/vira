{- | Bitbucket authentication status checking

Check if Bitbucket CLI is authenticated by verifying config file exists.
-}
module Bitbucket.Auth.Status (
  AuthStatus (..),
  checkAuthStatus,
) where

import Bitbucket.API.V1.Core (BitbucketConfig (..))
import Bitbucket.Config qualified as Config
import Network.HTTP.Req (renderUrl)

-- | Authentication status for Bitbucket CLI
data AuthStatus
  = Authenticated
      { serverUrl :: Text
      -- ^ Bitbucket server URL
      }
  | NotAuthenticated
  deriving stock (Show, Eq)

{- | Check if Bitbucket CLI is authenticated

Checks if the config file exists and is valid.
-}
checkAuthStatus :: IO AuthStatus
checkAuthStatus = do
  result <- Config.loadConfig
  pure $ case result of
    Right (BitbucketConfig baseUrl _) -> Authenticated {serverUrl = renderUrl baseUrl}
    Left _ -> NotAuthenticated
