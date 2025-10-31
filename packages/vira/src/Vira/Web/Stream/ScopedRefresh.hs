{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Entity-scoped Server-Sent Events for automatic page refresh.

Uses the event bus to subscribe to relevant updates for the current page,
triggers page reload when matching events occur.

Flow: breadcrumbs → 'pageEntityFilter' → 'viewStreamScoped' → SSE → 'streamRouteHandler' → reload
-}
module Vira.Web.Stream.ScopedRefresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStreamScoped,
  pageEntityFilter,

  -- * Typeclass (for App.update constraint)
  AffectedEntities (..),
) where

import Colog.Core (Severity (Debug, Error))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM qualified as STM
import Data.Acid (EventResult, UpdateEvent)
import Data.Acid.Events (SomeUpdate (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import Effectful.Concurrent (threadDelay)
import Effectful.Git (RepoName)
import Lucid
import Servant.API (QueryParam, SourceIO, type (:>))
import Servant.API.EventStream
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.State.Acid
import Vira.State.Core (ViraState)
import Vira.State.Type (Job (jobId), JobId, Repo (name))
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, filter, runReader)

-- * Entity filtering types

-- | Identifiers for entities in Vira
data EntityId
  = RepoId RepoName
  | JobId JobId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Typeclass for determining which entities an update affects (used for SSE filtering)
class (UpdateEvent event) => AffectedEntities event where
  affectedEntities :: event -> EventResult event -> Set EntityId
  affectedEntities _ _ = Set.empty

-- * AffectedEntities instances

instance AffectedEntities SetAllReposA where
  affectedEntities (SetAllReposA repos) _ =
    Set.fromList $ fmap (\r -> RepoId r.name) repos

instance AffectedEntities AddNewRepoA where
  affectedEntities (AddNewRepoA repo) _ =
    one (RepoId repo.name)

instance AffectedEntities DeleteRepoByNameA where
  affectedEntities (DeleteRepoByNameA name) (Right ()) =
    one (RepoId name)
  affectedEntities _ _ = Set.empty

instance AffectedEntities SetRepoA where
  affectedEntities (SetRepoA repo) _ =
    one (RepoId repo.name)

instance AffectedEntities SetRepoBranchesA where
  affectedEntities (SetRepoBranchesA name _) _ =
    one (RepoId name)

instance AffectedEntities StoreCommitA where
  -- Commits don't have direct entity scoping for SSE
  affectedEntities _ _ = Set.empty

instance AffectedEntities AddNewJobA where
  affectedEntities (AddNewJobA repo _ _ _ _) job =
    Set.fromList [RepoId repo, JobId job.jobId]

instance AffectedEntities JobUpdateStatusA where
  affectedEntities (JobUpdateStatusA jid _) _ =
    one (JobId jid)

instance AffectedEntities MarkUnfinishedJobsAsStaleA where
  -- This is internal, no SSE needed
  affectedEntities _ _ = Set.empty

-- * Show instances (for debug logging)

deriving stock instance Show SetAllReposA

deriving stock instance Show AddNewRepoA

deriving stock instance Show DeleteRepoByNameA

deriving stock instance Show GetAllReposA

deriving stock instance Show GetRepoByNameA

deriving stock instance Show GetAllBranchesA

deriving stock instance Show GetBranchByNameA

deriving stock instance Show GetBranchDetailsA

deriving stock instance Show SetRepoA

deriving stock instance Show SetRepoBranchesA

deriving stock instance Show GetCommitByIdA

deriving stock instance Show StoreCommitA

deriving stock instance Show GetJobsByBranchA

deriving stock instance Show GetRecentJobsA

deriving stock instance Show GetRunningJobs

deriving stock instance Show GetJobA

deriving stock instance Show AddNewJobA

deriving stock instance Show JobUpdateStatusA

deriving stock instance Show MarkUnfinishedJobsAsStaleA

-- * Filtering helpers

-- | Check if update affects a specific entity
affectsEntity :: EntityId -> SomeUpdate ViraState AffectedEntities -> Bool
affectsEntity entityId (SomeUpdate evt result _timestamp) =
  entityId `Set.member` affectedEntities evt result

-- | Check if update affects a specific repo
affectsRepo :: RepoName -> SomeUpdate ViraState AffectedEntities -> Bool
affectsRepo name = affectsEntity (RepoId name)

-- | Check if update affects a specific job
affectsJob :: JobId -> SomeUpdate ViraState AffectedEntities -> Bool
affectsJob jobId = affectsEntity (JobId jobId)

-- | Check if update affects any job
affectsAnyJob :: SomeUpdate ViraState AffectedEntities -> Bool
affectsAnyJob (SomeUpdate evt result _timestamp) =
  any isJobEntity (affectedEntities evt result)
  where
    isJobEntity (JobId _) = True
    isJobEntity _ = False

-- * SSE Routes and handlers

type StreamRoute = QueryParam "filter" Text :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO (KeepAlive ScopedRefresh)))

{- | A scoped refresh signal sent from server to client

Wrapped in @KeepAlive@ to add heartbeat support, preventing connection
timeout during long-running operations (e.g., builds taking 5+ minutes).
-}
data ScopedRefresh = ScopedRefresh

instance ToServerEvent ScopedRefresh where
  toServerEvent ScopedRefresh =
    ServerEvent
      (Just "refresh") -- Custom event type to distinguish from heartbeat "message" events
      Nothing -- Event ID
      "Refresh"

-- | Derive entity filter from breadcrumbs
pageEntityFilter :: [LinkTo] -> Maybe (Maybe EntityId)
pageEntityFilter crumbs = case reverse crumbs of
  (Job jobId : _) -> Just $ Just (JobId jobId)
  (RepoBranch repoName _ : _) -> Just $ Just (RepoId repoName)
  (Repo repoName : _) -> Just $ Just (RepoId repoName)
  [] -> Just Nothing -- Index page: subscribe to all job events
  _ -> Nothing -- No refresh for other pages

-- | SSE listener with event filtering
viewStreamScoped :: Maybe EntityId -> AppHtml ()
viewStreamScoped filter = do
  let filterParam = decodeUtf8 (encode filter)
  link <- lift $ getLinkUrl $ LinkTo.Refresh (Just filterParam)
  -- Use native EventSource for reliable refresh-triggered reloads
  script_ $
    "new EventSource('" <> link <> "').addEventListener('refresh', () => location.reload());"

data StreamConfig = StreamConfig
  { counter :: Int
  , filter :: Maybe EntityId
  , channel :: STM.TChan (SomeUpdate ViraState AffectedEntities)
  }

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) (KeepAlive ScopedRefresh)
streamRouteHandler mFilterParam = S.fromStepT $ S.Effect $ do
  tagStreamThread
  filter <- case eitherDecode . encodeUtf8 $ fromMaybe "null" mFilterParam of
    Left err -> do
      log Error $ "Invalid entity filter: " <> toText err
      pure Nothing -- Fall back to all jobs when parse fails
    Right f -> pure f
  log Debug $ "Starting stream with filter: " <> show filter
  chan <- App.subscribe
  pure $ step StreamConfig {counter = 0, filter, channel = chan}
  where
    step cfg =
      KeepAlive.withKeepAlive
        (waitForRelevantUpdate cfg.channel cfg.filter)
        ( \matchingUpdates -> do
            log Debug $ "Sending refresh for " <> show (length matchingUpdates) <> " matching events; n=" <> show cfg.counter
            pure $ Just (ScopedRefresh, cfg {counter = cfg.counter + 1})
        )
        cfg
        step

-- | Wait for and collect relevant updates (with debouncing)
waitForRelevantUpdate ::
  STM.TChan (SomeUpdate ViraState AffectedEntities) ->
  Maybe EntityId ->
  Eff AppStack (NonEmpty (SomeUpdate ViraState AffectedEntities))
waitForRelevantUpdate chan entityFilter = do
  -- Block until first relevant event
  firstUpdate <- waitForMatch
  -- Debounce: wait for more events to batch together
  threadDelay 2_500_000 -- 2.5 seconds
  -- Collect any additional matching events
  moreUpdates <- drainMatching []
  pure $ firstUpdate :| moreUpdates
  where
    -- Wait for an update that matches our filter
    waitForMatch :: Eff AppStack (SomeUpdate ViraState AffectedEntities)
    waitForMatch = liftIO $ STM.atomically $ do
      someUpdate <- STM.readTChan chan
      if matchesFilter entityFilter someUpdate
        then pure someUpdate
        else retry -- Retry in STM - keep reading until match

    -- Drain additional matching updates (non-blocking)
    drainMatching :: [SomeUpdate ViraState AffectedEntities] -> Eff AppStack [SomeUpdate ViraState AffectedEntities]
    drainMatching acc = do
      mUpdate <- liftIO $ STM.atomically $ STM.tryReadTChan chan
      case mUpdate of
        Nothing -> pure $ reverse acc
        Just someUpdate ->
          if matchesFilter entityFilter someUpdate
            then drainMatching (someUpdate : acc)
            else drainMatching acc -- Skip non-matching

-- | Check if update matches entity filter
matchesFilter :: Maybe EntityId -> SomeUpdate ViraState AffectedEntities -> Bool
matchesFilter Nothing update = affectsAnyJob update
matchesFilter (Just entityId) update = affectsEntity entityId update
