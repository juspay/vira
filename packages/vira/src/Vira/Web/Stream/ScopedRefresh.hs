{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

  -- * Entity types
  EntityId (..),
) where

import Colog.Core (Severity (Debug, Error))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM qualified as STM
import Data.Acid.Events (SomeUpdate (..), matchUpdate)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
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
import Vira.State.AcidInstances ()
import Vira.State.Core (ViraState)
import Vira.State.Type (Job (jobId), JobId, Repo (name))
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, filter, runReader)

-- * Entity types

-- | Identifiers for entities in Vira
data EntityId
  = RepoId RepoName
  | JobId JobId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  , channel :: STM.TChan (SomeUpdate ViraState)
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
  STM.TChan (SomeUpdate ViraState) ->
  Maybe EntityId ->
  Eff AppStack (NonEmpty (SomeUpdate ViraState))
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
    waitForMatch :: Eff AppStack (SomeUpdate ViraState)
    waitForMatch = liftIO $ STM.atomically $ do
      someUpdate <- STM.readTChan chan
      if matchesFilter entityFilter someUpdate
        then pure someUpdate
        else retry -- Retry in STM - keep reading until match

    -- Drain additional matching updates (non-blocking)
    drainMatching :: [SomeUpdate ViraState] -> Eff AppStack [SomeUpdate ViraState]
    drainMatching acc = do
      mUpdate <- liftIO $ STM.atomically $ STM.tryReadTChan chan
      case mUpdate of
        Nothing -> pure $ reverse acc
        Just someUpdate ->
          if matchesFilter entityFilter someUpdate
            then drainMatching (someUpdate : acc)
            else drainMatching acc -- Skip non-matching

-- | Check if update matches entity filter
matchesFilter :: Maybe EntityId -> SomeUpdate ViraState -> Bool
matchesFilter Nothing someUpdate =
  -- Match any job update
  any (\case JobId _ -> True; _ -> False) (extractEntityIds someUpdate)
matchesFilter (Just entityId) someUpdate =
  -- Match specific entity
  entityId `Set.member` extractEntityIds someUpdate

-- | Extract entity IDs from an update event (manual pattern matching)
extractEntityIds :: SomeUpdate ViraState -> Set EntityId
extractEntityIds someUpdate =
  -- SetAllReposA
  case matchUpdate @SetAllReposA someUpdate of
    Just (SetAllReposA repos, _) -> Set.fromList $ fmap (\r -> RepoId r.name) repos
    Nothing -> case matchUpdate @AddNewRepoA someUpdate of
      Just (AddNewRepoA repo, _) -> one (RepoId repo.name)
      Nothing -> case matchUpdate @DeleteRepoByNameA someUpdate of
        Just (DeleteRepoByNameA repoName, Right ()) -> one (RepoId repoName)
        Just (DeleteRepoByNameA _, Left _) -> Set.empty
        Nothing -> case matchUpdate @SetRepoA someUpdate of
          Just (SetRepoA repo, _) -> one (RepoId repo.name)
          Nothing -> case matchUpdate @SetRepoBranchesA someUpdate of
            Just (SetRepoBranchesA repoName _, _) -> one (RepoId repoName)
            Nothing -> case matchUpdate @StoreCommitA someUpdate of
              Just (StoreCommitA {}, _) -> Set.empty -- Commits don't have direct entity scoping
              Nothing -> case matchUpdate @AddNewJobA someUpdate of
                Just (AddNewJobA repoName _ _ _ _, job) -> Set.fromList [RepoId repoName, JobId job.jobId]
                Nothing -> case matchUpdate @JobUpdateStatusA someUpdate of
                  Just (JobUpdateStatusA jid _, _) -> one (JobId jid)
                  Nothing -> case matchUpdate @MarkUnfinishedJobsAsStaleA someUpdate of
                    Just (MarkUnfinishedJobsAsStaleA, _) -> Set.empty -- Internal, no SSE
                    Nothing -> Set.empty -- Unknown event type
