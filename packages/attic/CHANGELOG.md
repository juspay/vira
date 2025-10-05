# Changelog for attic

## Unreleased

- URL parsing utilities for cache URLs
- Configuration file parsing from `~/.config/attic/config.toml`
- API change
  - Add `AtticServerEndpoint` newtype for type safety
  - Rename `AtticServer.serverUrl` to `serverEndpoint :: AtticServerEndpoint`
  - Change `parseCacheUrl` to return `(AtticServerEndpoint, AtticCache)` instead of `(Text, AtticCache)`
  - Update `AtticConfig` to use `AtticServerEndpoint` and `AtticToken` newtypes

## 0.1.0.0 (2025-09-26)

Initial release with:

- Type-safe wrappers for Attic cache server, cache name, and token
- Process helpers for `attic login` and `attic push` commands
