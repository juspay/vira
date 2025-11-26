{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- | Git platform detection

Detect the git hosting platform from a remote URL.
-}
module Effectful.Git.Platform (
  GitPlatform (..),
  detectPlatform,
  parseAndDetect,
  getHostText,
) where

import Control.Lens ((^.), (^?), _Right)
import Data.Text qualified as T
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens qualified as URIL

-- | Supported git platforms
data GitPlatform
  = GitHub
  | -- | Bitbucket with base URL
    Bitbucket Text
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- 1. Normalization Logic
--------------------------------------------------------------------------------

{- | Converts SCP-style Git URLs to standard SSH URIs.

Input:  git@ssh.bitbucket.juspay.net:xyne/xyne-spaces.git
Output: ssh://git@ssh.bitbucket.juspay.net/xyne/xyne-spaces.git

Input:  https://github.com/user/repo.git
Output: https://github.com/user/repo.git (Unchanged)
-}
normalizeGitUrl :: Text -> Text
normalizeGitUrl raw
  | "://" `T.isInfixOf` raw = raw -- Already a valid URI with scheme
  | otherwise =
      -- Attempt to detect SCP pattern "user@host:path" or "host:path"
      -- We look for the first colon.
      case T.breakOn ":" raw of
        (before, after)
          -- If there is no colon, it's likely a local path (e.g. "/home/user/repo")
          -- modern-uri might fail to parse this as a network URI, but that's expected.
          | T.null after -> raw
          -- We found a colon.
          -- SCP Syntax: host:path -> URI Syntax: ssh://host/path
          -- We drop 1 to remove the ':' and replace it with '/' (prefixed by scheme)
          | otherwise -> "ssh://" <> before <> "/" <> T.drop 1 after

--------------------------------------------------------------------------------
-- 2. Parsing Logic
--------------------------------------------------------------------------------

-- | Extracts the host from the URI structure using lenses
getHostText :: URI -> Maybe Text
getHostText uri = do
  auth <- uri ^? URIL.uriAuthority . _Right
  let host = auth ^. URIL.authHost
  pure (URI.unRText host)

-- | Detects platform based on the host
detectPlatform :: URI -> Maybe GitPlatform
detectPlatform uri =
  case getHostText uri of
    Nothing -> Nothing
    Just host ->
      if "github.com" `T.isInfixOf` host
        then Just GitHub
        else
          if "bitbucket" `T.isInfixOf` host
            then Just (Bitbucket $ stripSSHPrefix host)
            else Nothing

-- | Strip "ssh." prefix from hostname for API usage
stripSSHPrefix :: Text -> Text
stripSSHPrefix host =
  if "ssh." `T.isPrefixOf` host
    then T.drop 4 host
    else host

-- | Main Entry Point: Handles SCP, HTTP, SSH, Git, etc.
parseAndDetect :: Text -> Maybe GitPlatform
parseAndDetect raw = do
  -- 1. Normalize SCP syntax to SSH syntax
  let normalized = normalizeGitUrl raw

  -- 2. Parse strictly with modern-uri
  uri <- URI.mkURI normalized

  -- 3. Detect Platform
  detectPlatform uri
