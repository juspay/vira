{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub CLI authentication status checking

Provides functionality to check if the user is authenticated with GitHub CLI.
-}
module GH.Auth.Status (
  AuthStatus (..),
  AuthEntryState (..),
  checkAuthStatus,
) where

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (UntaggedValue), defaultOptions, genericParseJSON)
import Data.Aeson qualified as Aeson
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GH.Core (ghBin)
import System.Process.Typed qualified as P

-- | GitHub CLI authentication status
data AuthStatus
  = Authenticated
      { host :: Text
      , login :: Text
      , scopes :: [Text]
      }
  | NotAuthenticated
  deriving stock (Show, Eq)

-- | Authentication entry state from gh CLI
data AuthEntryState
  = Success
  | Timeout
  | Error
  deriving stock (Show, Eq, Generic)

instance FromJSON AuthEntryState where
  parseJSON = genericParseJSON opts
    where
      opts =
        defaultOptions
          { constructorTagModifier = map toLower
          , sumEncoding = UntaggedValue
          }

-- | JSON response structure from gh auth status
newtype AuthResponse = AuthResponse
  { hosts :: Map Text [HostAuth]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data HostAuth = HostAuth
  { state :: AuthEntryState
  , error :: Maybe Text
  , active :: Bool
  , host :: Text
  , login :: Text
  , tokenSource :: Text
  , token :: Maybe Text
  , scopes :: Maybe Text
  , gitProtocol :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Check GitHub CLI authentication status
checkAuthStatus :: IO AuthStatus
checkAuthStatus = do
  output <- P.readProcessStdout_ $ P.proc ghBin ["auth", "status", "--json", "hosts"]

  case Aeson.decode output of
    Nothing -> pure NotAuthenticated
    Just (AuthResponse hostsMap) -> do
      case Map.lookup "github.com" hostsMap of
        Nothing -> pure NotAuthenticated
        Just auths -> do
          -- Find active auth with success state
          case find (\auth -> auth.active && auth.state == Success && auth.host == "github.com") auths of
            Nothing -> pure NotAuthenticated
            Just auth ->
              pure $
                Authenticated
                  { host = auth.host
                  , login = auth.login
                  , scopes = maybe [] (map T.strip . T.splitOn ",") auth.scopes
                  }
