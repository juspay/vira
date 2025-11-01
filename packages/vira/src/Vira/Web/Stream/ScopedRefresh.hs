{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Entity-scoped Server-Sent Events for automatic page refresh.

Uses the event bus to subscribe to relevant updates for the current page,
triggers page reload when matching events occur.

Flow: breadcrumbs → 'pageEntity' → 'viewStreamScoped' → SSE → 'streamRouteHandler' → reload
-}
module Vira.Web.Stream.ScopedRefresh (
  -- * Views
  viewStreamScoped,

  -- * Handlers
  streamRouteHandler,
  StreamRoute,
) where

import Colog.Core (Severity (Debug, Error))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM qualified as STM
import Data.Acid.Events (SomeUpdate (..), matchUpdate)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
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
import Vira.Web.LinkTo.Type (LinkTo)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, filter, runReader)

-- * Views

-- | Add SSE listener for auto-refresh based on page breadcrumbs
viewStreamScoped :: [LinkTo] -> AppHtml ()
viewStreamScoped crumbs =
  whenJust (pageEntity crumbs) $ \entity -> do
    let filterParam = decodeUtf8 (encode entity)
    link <- lift $ getLinkUrl $ LinkTo.Refresh (Just filterParam)
    -- Use native EventSource for reliable refresh-triggered reloads
    script_ $
      "new EventSource('" <> link <> "').addEventListener('refresh', () => location.reload());"

-- | Derive entity filter from breadcrumbs (internal)
pageEntity :: [LinkTo] -> Maybe Entity
pageEntity crumbs = case reverse crumbs of
  (LinkTo.Job jobId : _) -> Just (Job jobId)
  (LinkTo.RepoBranch repoName _ : _) -> Just (Repo repoName)
  (LinkTo.Repo repoName : _) -> Just (Repo repoName)
  [] -> Just AnyEntity -- Index page = match all updates
  _ -> Nothing -- No refresh for other pages

-- * Handlers

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) (KeepAlive ScopedRefresh)
streamRouteHandler mFilterParam = S.fromStepT $ S.Effect $ do
  tagStreamThread
  entity <- case eitherDecode . encodeUtf8 $ fromMaybe "null" mFilterParam of
    Left err -> do
      log Error $ "Invalid entity filter: " <> toText err
      pure AnyEntity
    Right e -> pure e
  log Debug $ "Starting stream with filter: " <> show entity
  chan <- App.subscribe
  pure $ step StreamConfig {counter = 0, entity, channel = chan}
  where
    step cfg =
      KeepAlive.withKeepAlive
        (waitForRelevantUpdate cfg.channel cfg.entity)
        ( \matchingUpdates -> do
            log Debug $ "Sending refresh for " <> show (length matchingUpdates) <> " matching events; n=" <> show cfg.counter
            pure $ Just (ScopedRefresh, cfg {counter = cfg.counter + 1})
        )
        cfg
        step

-- | Wait for and collect relevant updates (with debouncing)
waitForRelevantUpdate ::
  STM.TChan (SomeUpdate ViraState) ->
  Entity ->
  Eff AppStack (NonEmpty (SomeUpdate ViraState))
waitForRelevantUpdate chan entity = do
  -- Block until first relevant event
  firstUpdate <- waitForMatch
  -- Debounce: wait for more events to batch together
  threadDelay 2_500_000 -- 2.5 seconds
  -- Collect any additional matching events
  moreUpdates <- drainMatching []
  pure $ firstUpdate :| moreUpdates
  where
    predicate = buildPredicate entity

    -- Wait for an update that matches our filter
    waitForMatch :: Eff AppStack (SomeUpdate ViraState)
    waitForMatch = liftIO $ STM.atomically $ do
      someUpdate <- STM.readTChan chan
      if predicate someUpdate
        then pure someUpdate
        else retry -- Retry in STM - keep reading until match

    -- Drain additional matching updates (non-blocking)
    drainMatching :: [SomeUpdate ViraState] -> Eff AppStack [SomeUpdate ViraState]
    drainMatching acc = do
      mUpdate <- liftIO $ STM.atomically $ STM.tryReadTChan chan
      case mUpdate of
        Nothing -> pure $ reverse acc
        Just someUpdate ->
          if predicate someUpdate
            then drainMatching (someUpdate : acc)
            else drainMatching acc -- Skip non-matching

-- | Build predicate from entity filter
buildPredicate :: Entity -> (SomeUpdate ViraState -> Bool)
buildPredicate AnyEntity = const True
buildPredicate entity = (entity `elem`) . entitiesChanged

-- | Extract entities affected by an update
entitiesChanged :: SomeUpdate ViraState -> [Entity]
entitiesChanged update
  | Just (SetAllReposA repos, _) <- matchUpdate update =
      Repo . (.name) <$> repos
  | Just (AddNewRepoA repo, _) <- matchUpdate update =
      [Repo repo.name]
  | Just (DeleteRepoByNameA name, Right ()) <- matchUpdate update =
      [Repo name]
  | Just (SetRepoA repo, _) <- matchUpdate update =
      [Repo repo.name]
  | Just (SetRepoBranchesA name _, _) <- matchUpdate update =
      [Repo name]
  | Just (AddNewJobA repo _ _ _ _, job) <- matchUpdate update =
      [Repo repo, Job job.jobId]
  | Just (JobUpdateStatusA jid _, _) <- matchUpdate update =
      [Job jid]
  | otherwise = []

-- * Internal types

data StreamConfig = StreamConfig
  { counter :: Int
  , entity :: Entity
  , channel :: STM.TChan (SomeUpdate ViraState)
  }

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

type StreamRoute = QueryParam "filter" Text :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO (KeepAlive ScopedRefresh)))

-- * Foundation types

-- | Identifiers for entities in Vira
data Entity
  = Repo RepoName
  | Job JobId
  | AnyEntity -- Match all updates
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
