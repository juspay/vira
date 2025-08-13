# FileTailer Refactoring Plan

This document outlines refactoring opportunities for `src/Vira/Lib/FileTailer.hs` to prepare it for release as a separate Haskell library.

## 1. Configuration & Parameterization

**Current Issues:**
- Fixed to `Text` type, could be generic over line content type
- Non-configurable startup/shutdown messages

**Refactoring:**
Further generalize over content types:
```haskell
-- Could support different content types beyond Text
data FileTailer (recentLines :: Nat) (batchSize :: Nat) (queueSize :: Nat) content = FileTailer
  { ...
  , clientQueues :: STM.TVar [TBQueue (SizedBatch batchSize content)]
  , recentLines :: STM.TVar (CircularBuffer recentLines content)
  }

-- Configurable messages
data FileTailerConfig content = FileTailerConfig
  { startupMessage :: Maybe content
  , shutdownMessage :: Maybe content
  , errorMessageFormat :: SomeException -> content
  }
```

**Status:** ✅ Hard-coded values (100, 50, 100) have been moved to runtime configuration with FileTailerConfig.

## 2. Type Generalization

**Current Issues:**
- Fixed to `NonEmpty Text` for batching
- Hard-coded to `Text` for line content

**Refactoring:**
```haskell
-- Generic over line content type and batch type
data FileTailer batch line = FileTailer { ... }

-- Support different batching strategies
class Batchable batch line where
  makeBatch :: [line] -> [batch]
  fromBatch :: batch -> [line]
```

## 3. Dependency Separation

**Current Issues:**
- Depends on `Vira.Lib.CircularBuffer` (should be bundled or made optional)
- Uses relude-specific functions
- Mixed with application-specific HTML comments

**Refactoring:**
- Make CircularBuffer dependency optional via type class
- Use only base + standard libraries
- Remove HTML comment messages (make configurable)

## 4. Error Handling & Logging

**Current Issues:**
- Uses generic `SomeException` catching
- Hard-coded error messages
- No structured logging interface

**Refactoring:**
```haskell
class TailerLogger m where
  logTailerEvent :: TailerEvent -> m ()

data TailerEvent 
  = TailerStarted FilePath
  | TailerStopped FilePath  
  | TailerError FilePath SomeException
  | FileRotationDetected FilePath
```

## 5. Resource Management

**Current Issues:**
- Manual thread management with `forkIO`
- No proper resource cleanup guarantees
- Mixed concerns (file watching + broadcasting)

**Refactoring:**
```haskell
-- Separate concerns into composable parts
data FileWatcher = FileWatcher { ... }
data LineBroadcaster batch line = LineBroadcaster { ... }

-- Use bracket-style resource management
withFileTailer :: FileTailerConfig -> FilePath -> (FileTailer -> IO a) -> IO a
```

## 6. API Simplification

**Current Issues:**
- Exposes STM internals (`TBQueue`, `STM.atomically`)
- Complex subscription model
- Mixed sync/async operations

**Refactoring:**
```haskell
-- Hide STM implementation details
newtype TailHandle batch = TailHandle (TBQueue batch)

-- Simpler callback-based API option
startTailingWithCallback :: FilePath -> (batch -> IO ()) -> IO FileTailer
```

## 7. Testing & Modularity

**Current Issues:**
- Tightly coupled to filesystem events
- Hard to test without actual files
- Single monolithic module

**Refactoring:**
```haskell
-- Abstract file operations for testing
class FileOps m where
  readFileFrom :: FilePath -> Integer -> m (ByteString, Integer)
  watchFile :: FilePath -> (FilePath -> m ()) -> m (m ())

-- Split into multiple modules
-- * FileTailer.Core (main types)
-- * FileTailer.FileSystem (fsnotify integration)  
-- * FileTailer.Broadcasting (client management)
-- * FileTailer.Config (configuration)
```

## 8. Performance Configurability

**Current Issues:**
- Fixed polling/retry behavior
- No backpressure handling configuration
- Hard-coded buffer sizes

**Refactoring:**
```haskell
data PerformanceConfig = PerformanceConfig
  { retryDelayMicros :: Int
  , maxBacklogSize :: Natural
  , backpressureStrategy :: BackpressureStrategy
  }

data BackpressureStrategy = DropOldest | DropNewest | Block
```

## 9. Documentation & Examples

**Current Gaps:**
- No examples for common use cases
- Limited performance characteristics documentation
- No migration guide from `tail -f`

**Additions Needed:**
- Comprehensive Haddock documentation
- Performance benchmarks vs alternatives
- Real-world usage examples
- Integration guides for common frameworks

## Summary

The key insight is that the current implementation is quite good but too specialized for the Vira use case. The refactoring should focus on **generalization**, **configurability**, and **separation of concerns** while maintaining the efficient STM-based architecture.