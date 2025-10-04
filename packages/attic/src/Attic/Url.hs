-- | Attic cache URL parsing utilities
module Attic.Url (
  parseCacheUrl,
) where

import Data.Text qualified as T
import Text.URI qualified as URI

{- | Parse cache URL like "https://cache.nixos.asia/oss"

Returns (serverEndpoint, cacheName) where:
- serverEndpoint is the base URL without path (e.g., "https://cache.nixos.asia")
- cacheName is the last path segment (e.g., "oss")
-}
parseCacheUrl :: Text -> Either String (Text, Text)
parseCacheUrl urlText = do
  -- Parse using modern-uri
  uri <- first show $ URI.mkURI urlText

  -- Extract path segments for cache name
  pathParts <- case URI.uriPath uri of
    Nothing -> Left "Cache URL must have a path"
    Just (_isAbsolute, pathSegments) ->
      let segments = map URI.unRText (toList pathSegments)
          nonEmptySegments = filter (not . T.null) segments
       in case nonEmpty nonEmptySegments of
            Nothing -> Left "Cache URL must have a path segment for the cache name"
            Just ne -> Right ne
  let cacheName = last pathParts

  -- Render server endpoint without the path
  let serverEndpoint = URI.render $ uri {URI.uriPath = Nothing}
  Right (serverEndpoint, cacheName)
