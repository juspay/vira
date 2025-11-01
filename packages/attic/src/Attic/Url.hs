-- | Attic cache URL parsing utilities
module Attic.Url (
  parseCacheUrl,
  ParseError (..),
) where

import Attic.Types (AtticCache (..), AtticServerEndpoint (..))
import Text.Megaparsec (parse)
import Text.URI qualified as URI

-- | Errors that can occur when parsing cache URLs
data ParseError
  = -- | The URL could not be parsed as a valid URI
    InvalidURI URI.ParseException
  | -- | The URL has no path component
    MissingPath
  | -- | The URL has multiple path segments (must have exactly one for cache name)
    MultiplePath (NonEmpty Text)
  deriving stock (Show, Eq)

{- | Parse cache URL like @https:\/\/cache.nixos.asia\/oss@

Returns @(serverEndpoint, cacheName)@ where:
- @serverEndpoint@ is the base URL without path (e.g., @https:\/\/cache.nixos.asia@)
- @cacheName@ is the last path segment (e.g., @oss@)
-}
parseCacheUrl :: Text -> Either ParseError (AtticServerEndpoint, AtticCache)
parseCacheUrl urlText = do
  -- Parse using modern-uri's parser directly via megaparsec
  uri <- first (InvalidURI . URI.ParseException) $ parse URI.parser "" urlText

  -- Extract path segments for cache name - must have exactly 1 segment
  cacheName <- case URI.uriPath uri of
    Nothing -> Left MissingPath
    Just (_isAbsolute, pathSegments) ->
      case fmap URI.unRText pathSegments of
        single :| [] -> Right $ AtticCache single
        segments' -> Left $ MultiplePath segments'

  -- Render server endpoint without the path
  let serverEndpoint = AtticServerEndpoint $ URI.render $ uri {URI.uriPath = Nothing}
  Right (serverEndpoint, cacheName)
