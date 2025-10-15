# co-log-effectful-simple

Simple context-aware logging for [co-log](https://hackage.haskell.org/package/co-log) with [effectful](https://hackage.haskell.org/package/effectful).

## Overview

This library combines `co-log`'s `RichMessage` with `effectful`'s `Reader` effect to provide context-aware structured logging with minimal ceremony.

## Features

- **Context accumulation**: Add key-value pairs that automatically propagate to nested operations
- **Thread labeling**: Tag threads with human-readable labels for better debugging
- **Rich formatting**: Timestamps, severity colors, thread IDs, and custom context in log output
- **Composable effects**: Uses standard `Log (RichMessage IO)` and `Reader LogContext` effects

## Usage

```haskell
import Effectful.Colog.Simple

main :: IO ()
main = runEff . runLogActionStdout Info $ do
  log Info "Starting application"

  withLogContext [("user", "alice"), ("action", "login")] $ do
    log Info "Processing request"  -- Includes {user=alice, action=login}

    withLogContext [("step", "validate")] $ do
      log Debug "Checking credentials"  -- Includes all three context pairs
```

## API

### Logging

- `log :: Severity -> Text -> Eff es ()` - Log a message with context
- `withLogContext :: [(Text, Text)] -> Eff es a -> Eff es a` - Add context to nested operations
- `tagCurrentThread :: String -> m ()` - Label current thread for debugging

### Running

- `runLogActionStdout :: Severity -> Eff '[Reader LogContext, Log (RichMessage IO), IOE] a -> Eff '[IOE] a` - Run with stdout output

### Types

- `LogContext` - Accumulated key-value context (Semigroup/Monoid)
- `Severity` - Log levels (Debug, Info, Warning, Error) from `co-log-core`

## Design

Rather than creating a custom effect, this library composes two orthogonal concerns:

1. **`Log (RichMessage IO)`** - The logging mechanism (from co-log-effectful)
2. **`Reader LogContext`** - The contextual data (from effectful)
