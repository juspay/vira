{-# LANGUAGE DuplicateRecordFields #-}

-- | In-process cache for GitHub pull request lookups.
module Vira.Web.PullRequestCache (
  PullRequestCache,
  PullRequestCacheResult (..),
  PullRequestCacheSource (..),
  newPullRequestCache,
  newPullRequestCacheWithTtl,
  newPullRequestCacheWithTtlAndMaxEntries,
  resolvePullRequest,
  resolvePullRequestWith,
) where

import Control.Concurrent.STM qualified as STM
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Effectful.Git qualified as Git
import GH.Core qualified as GH

data PullRequestCache = PullRequestCache
  { entries :: STM.TVar (Map PullRequestCacheKey PullRequestCacheEntry)
  , ttl :: NominalDiffTime
  , maxEntries :: Int
  }

data PullRequestCacheKey = PullRequestCacheKey
  { cloneUrl :: Text
  , branchName :: Git.BranchName
  }
  deriving stock (Eq, Ord)

data PullRequestCacheEntry = PullRequestCacheEntry
  { fetchedAt :: UTCTime
  , result :: GH.PullRequestLookup
  }

-- | Whether a pull request lookup came from cache or from @gh@.
data PullRequestCacheSource = PullRequestCacheHit | PullRequestCacheMiss
  deriving stock (Show, Eq)

-- | Pull request lookup result with cache source metadata.
data PullRequestCacheResult = PullRequestCacheResult
  { result :: GH.PullRequestLookup
  , source :: PullRequestCacheSource
  }
  deriving stock (Show, Eq)

-- | Create a pull request cache with the default TTL.
newPullRequestCache :: IO PullRequestCache
newPullRequestCache =
  newPullRequestCacheWithTtl 60

-- | Create a pull request cache with a custom TTL.
newPullRequestCacheWithTtl :: NominalDiffTime -> IO PullRequestCache
newPullRequestCacheWithTtl ttl = do
  newPullRequestCacheWithTtlAndMaxEntries ttl 256

-- | Create a pull request cache with a custom TTL and entry cap.
newPullRequestCacheWithTtlAndMaxEntries :: NominalDiffTime -> Int -> IO PullRequestCache
newPullRequestCacheWithTtlAndMaxEntries ttl maxEntries = do
  entries <- STM.newTVarIO mempty
  pure PullRequestCache {entries, ttl, maxEntries}

-- | Resolve the pull request associated with a branch, using the cache first.
resolvePullRequest :: PullRequestCache -> Text -> Git.BranchName -> IO PullRequestCacheResult
resolvePullRequest cache cloneUrl branchName =
  resolvePullRequestWith cache cloneUrl branchName $
    GH.lookupForBranchFromCloneUrl cloneUrl (toText branchName)

-- | Resolve a pull request with an injected fetch action.
resolvePullRequestWith ::
  PullRequestCache ->
  Text ->
  Git.BranchName ->
  IO GH.PullRequestLookup ->
  IO PullRequestCacheResult
resolvePullRequestWith cache cloneUrl branchName fetch = do
  now <- getCurrentTime
  let key = PullRequestCacheKey {cloneUrl, branchName}
  cachedEntry <- Map.lookup key <$> STM.readTVarIO cache.entries
  case cachedEntry of
    Just entry
      | now `diffUTCTime` entry.fetchedAt < cache.ttl ->
          pure PullRequestCacheResult {result = entry.result, source = PullRequestCacheHit}
    _ -> do
      result <- fetch
      fetchedAt <- getCurrentTime
      STM.atomically $
        STM.modifyTVar' cache.entries $
          pruneEntries fetchedAt cache.ttl cache.maxEntries
            . Map.insert key PullRequestCacheEntry {fetchedAt, result}
      pure PullRequestCacheResult {result, source = PullRequestCacheMiss}

pruneEntries ::
  UTCTime ->
  NominalDiffTime ->
  Int ->
  Map PullRequestCacheKey PullRequestCacheEntry ->
  Map PullRequestCacheKey PullRequestCacheEntry
pruneEntries now ttl maxEntries =
  Map.fromList
    . take maxEntries
    . sortOn (Down . (.fetchedAt) . snd)
    . Map.toList
    . Map.filter (\entry -> now `diffUTCTime` entry.fetchedAt < ttl)
