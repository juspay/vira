{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- | Git platform detection from remote URLs

This module detects the hosting platform (GitHub, Bitbucket) from git remote URLs.
Supports various URL formats: HTTPS, SSH, and SCP-style git URLs.

= Usage

@
import Effectful.Git.Platform (GitPlatform(..), detectPlatform)

-- HTTPS URL
detectPlatform "https://github.com/user/repo.git"
-- Just GitHub

-- SSH URL (SCP-style)
detectPlatform "git@github.com:user/repo.git"
-- Just GitHub

-- Bitbucket SSH URL
detectPlatform "git@ssh.bitbucket.example.com:project/repo.git"
-- Just (Bitbucket "bitbucket.example.com")

-- Unknown platform
detectPlatform "git@gitlab.com:user/repo.git"
-- Nothing
@
-}
module Effectful.Git.Platform (
  GitPlatform (..),
  detectPlatform,
) where

import Control.Lens ((^.), (^?), _Right)
import Data.Text qualified as T
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens qualified as URIL

{- | Git hosting platform

Represents supported git hosting platforms detected from remote URLs.
-}
data GitPlatform
  = -- | GitHub platform (github.com)
    GitHub
  | {- | Bitbucket platform with hostname (e.g., @bitbucket.example.com@).
    The hostname has @ssh.@ prefix stripped if present.
    -}
    Bitbucket Text
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- 1. Normalization Logic
--------------------------------------------------------------------------------

{- | Normalize git URLs to standard URI format

Converts SCP-style git URLs (@user\@host:path@) to SSH URIs (@ssh:\/\/user\@host\/path@).
URLs with explicit schemes (HTTPS, SSH) pass through unchanged.

Examples:

@
normalizeGitUrl "git@github.com:user/repo.git"
-- "ssh://git@github.com/user/repo.git"

normalizeGitUrl "https://github.com/user/repo.git"
-- "https://github.com/user/repo.git" (unchanged)

normalizeGitUrl "/local/path/to/repo"
-- "/local/path/to/repo" (unchanged)
@
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

{- | Extract hostname from URI

Uses lens traversal to safely extract the host component.
Returns 'Nothing' if the URI has no authority section.
-}
getHostText :: URI -> Maybe Text
getHostText uri = do
  auth <- uri ^? URIL.uriAuthority . _Right
  let host = auth ^. URIL.authHost
  pure (URI.unRText host)

{- | Detect platform from parsed URI

Examines the hostname to determine the platform:

- Contains @github.com@ → 'GitHub'
- Contains @bitbucket@ → 'Bitbucket' (with @ssh.@ prefix stripped)
- Otherwise → 'Nothing'
-}
detectPlatformFromURI :: URI -> Maybe GitPlatform
detectPlatformFromURI uri =
  case getHostText uri of
    Nothing -> Nothing
    Just host ->
      if "github.com" `T.isInfixOf` host
        then Just GitHub
        else
          if "bitbucket" `T.isInfixOf` host
            then Just (Bitbucket $ stripSSHPrefix host)
            else Nothing

{- | Strip @ssh.@ prefix from hostname

Bitbucket SSH URLs often use @ssh.bitbucket.example.com@ but the API
endpoint is just @bitbucket.example.com@. This strips the prefix.

Examples:

@
stripSSHPrefix "ssh.bitbucket.juspay.net"
-- "bitbucket.juspay.net"

stripSSHPrefix "bitbucket.org"
-- "bitbucket.org" (unchanged)
@
-}
stripSSHPrefix :: Text -> Text
stripSSHPrefix host =
  if "ssh." `T.isPrefixOf` host
    then T.drop 4 host
    else host

{- | Detect git hosting platform from remote URL

Supports multiple URL formats (HTTPS, SSH, SCP-style) and normalizes them before detection.

Returns:

- @Just GitHub@ for github.com hosts
- @Just (Bitbucket host)@ for bitbucket hosts (with @ssh.@ prefix stripped)
- @Nothing@ for unsupported platforms or invalid URLs

Examples:

@
detectPlatform "git@github.com:user/repo.git"
-- Just GitHub

detectPlatform "https://bitbucket.example.com/project/repo.git"
-- Just (Bitbucket "bitbucket.example.com")

detectPlatform "git@ssh.bitbucket.juspay.net:project/repo.git"
-- Just (Bitbucket "bitbucket.juspay.net")

detectPlatform "git@gitlab.com:user/repo.git"
-- Nothing
@
-}
detectPlatform :: Text -> Maybe GitPlatform
detectPlatform =
  normalizeGitUrl -- 1. Normalize SCP syntax to SSH syntax
    >>> URI.mkURI -- 2. Parse strictly with modern-uri
    >=> detectPlatformFromURI -- 3. Detect platform from parsed URI
