-- | Attic cache URL parsing utilities
module Attic.Url (
  parseCacheUrl,
  ParseError (..),
) where

import Data.Text qualified as T
import Text.Megaparsec (parse)
import Text.URI qualified as URI

-- | Errors that can occur when parsing cache URLs
data ParseError
  = -- | The URL could not be parsed as a valid URI
    InvalidURI URI.ParseException
  | -- | The URL has no path component
    MissingPath
  | -- | The URL path is empty (no cache name)
    MissingCacheName
  | -- | The URL has multiple path segments (must have exactly one for cache name)
    MultiplePath (NonEmpty Text)
  deriving stock (Show, Eq)

{- | Parse cache URL like "https://cache.nixos.asia/oss"

Returns (serverEndpoint, cacheName) where:
- serverEndpoint is the base URL without path (e.g., "https://cache.nixos.asia")
- cacheName is the last path segment (e.g., "oss")
-}
parseCacheUrl :: Text -> Either ParseError (Text, Text)
parseCacheUrl urlText = do
  -- Parse using modern-uri's parser directly via megaparsec
  uri <- first (InvalidURI . URI.ParseException) $ parse URI.parser "" urlText

  -- Extract path segments for cache name - must have exactly 1 segment
  cacheName <- case URI.uriPath uri of
    Nothing -> Left MissingPath
    Just (_isAbsolute, pathSegments) ->
      let segments = map URI.unRText (toList pathSegments)
          nonEmptySegments = filter (not . T.null) segments
       in case nonEmptySegments of
            [] -> Left MissingCacheName
            [single] -> Right single
            (h : t) -> Left $ MultiplePath (h :| t)

  -- Render server endpoint without the path
  let serverEndpoint = URI.render $ uri {URI.uriPath = Nothing}
  Right (serverEndpoint, cacheName)
