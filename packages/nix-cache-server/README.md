# nix-cache-server

Minimal WAI application wrapper around [nix-serve-ng](https://github.com/aristanetworks/nix-serve-ng) for serving Nix binary caches.

## What it provides

- `makeCacheServer`: Creates a WAI application that serves Nix binary cache protocol
- `cacheMiddleware`: Mounts the cache at a path prefix (e.g., `/cache/*`)

## How it uses nix-serve-ng

This package is a thin wrapper that:

1. Initializes the Nix store using the `nix` Haskell library
2. Configures `nix-serve-ng` with store path and signing keys
3. Returns a WAI `Application` that handles standard Nix cache endpoints:
   - `/nix-cache-info` - Cache metadata
   - `/*.narinfo` - NAR info files
   - `/nar/*.nar` - NAR archives

All Nix protocol handling and NAR signing is delegated to `nix-serve-ng`.

## Usage

```haskell
import System.Nix.Cache.Server
import System.Nix.Cache.Keys

-- Load or generate cache signing keys
keys <- ensureCacheKeys "/path/to/keys"

-- Create cache application
cacheApp <- makeCacheServer keys.secretKey

-- Mount at /cache/* in your WAI server
let app = cacheMiddleware "cache" cacheApp mainApp
```
