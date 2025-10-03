{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub CLI authentication status checking

Provides functionality to check if the user is authenticated with GitHub CLI.
-}
module GH.Auth.Status (
  AuthStatus (..),
  checkAuthStatus,
) where

import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GH.Core (ghBin)
import System.Process (readProcess)

-- | GitHub CLI authentication status
data AuthStatus
  = Authenticated
      { host :: Text
      , login :: Text
      , scopes :: [Text]
      }
  | NotAuthenticated
  deriving stock (Show, Eq)

-- | JSON response structure from gh auth status
newtype AuthResponse = AuthResponse
  { hosts :: Map Text [HostAuth]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data HostAuth = HostAuth
  { state :: Text
  , active :: Bool
  , host :: Text
  , login :: Text
  , scopesRaw :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON HostAuth where
  parseJSON = Aeson.withObject "HostAuth" $ \v ->
    HostAuth
      <$> v .: "state"
      <*> v .: "active"
      <*> v .: "host"
      <*> v .: "login"
      <*> v .: "scopes"

-- | Check GitHub CLI authentication status
checkAuthStatus :: IO AuthStatus
checkAuthStatus = do
  output <- readProcess ghBin ["auth", "status", "--json", "hosts"] ""

  case Aeson.decode (encodeUtf8 $ toText output) of
    Nothing -> pure NotAuthenticated
    Just (AuthResponse hostsMap) -> do
      case Map.lookup "github.com" hostsMap of
        Nothing -> pure NotAuthenticated
        Just auths -> do
          -- Find active auth with success state
          case find (\auth -> auth.active && auth.state == "success" && auth.host == "github.com") auths of
            Nothing -> pure NotAuthenticated
            Just auth ->
              pure $
                Authenticated
                  { host = auth.host
                  , login = auth.login
                  , scopes = map T.strip $ T.splitOn "," auth.scopesRaw
                  }
