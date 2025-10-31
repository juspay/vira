{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
) where

import Colog.Core (Severity (Debug, Error))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM qualified as STM
import Data.Acid.Events (SomeUpdate)
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
import Vira.App.Event.Entity (AffectedEntities)
import Vira.App.Event.Entity qualified as Entity
import Vira.App.Stack (AppStack)
import Vira.State.Core (ViraState)
import Vira.State.Type (JobId)
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, filter, runReader)

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

-- | Entity filter for SSE - determines which updates are relevant
data EntityFilter
  = -- | Filter by specific repo
    FilterRepo RepoName
  | -- | Filter by specific job
    FilterJob JobId
  | -- | Match any job update (for home page)
    FilterAnyJob
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Derive entity filter from breadcrumbs
pageEntityFilter :: [LinkTo] -> Maybe EntityFilter
pageEntityFilter crumbs = case reverse crumbs of
  (Job jobId : _) -> Just $ FilterJob jobId
  (RepoBranch repoName _ : _) -> Just $ FilterRepo repoName
  (Repo repoName : _) -> Just $ FilterRepo repoName
  [] -> Just FilterAnyJob -- Index page: subscribe to all job events
  _ -> Nothing -- No refresh for other pages

-- | SSE listener with event filtering
viewStreamScoped :: EntityFilter -> AppHtml ()
viewStreamScoped filter = do
  let filterParam = decodeUtf8 (encode filter)
  link <- lift $ getLinkUrl $ LinkTo.Refresh (Just filterParam)
  -- Use native EventSource for reliable refresh-triggered reloads
  script_ $
    "new EventSource('" <> link <> "').addEventListener('refresh', () => location.reload());"

data StreamConfig = StreamConfig
  { counter :: Int
  , filter :: EntityFilter
  , channel :: STM.TChan (SomeUpdate ViraState AffectedEntities)
  }

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) (KeepAlive ScopedRefresh)
streamRouteHandler mFilterParam = S.fromStepT $ S.Effect $ do
  tagStreamThread
  filter <- case eitherDecode . encodeUtf8 $ fromMaybe "null" mFilterParam of
    Left err -> do
      log Error $ "Invalid entity filter: " <> toText err
      pure FilterAnyJob -- Fall back to all jobs when parse fails
    Right Nothing -> do
      log Error "Missing entity filter"
      pure FilterAnyJob
    Right (Just f) -> pure f
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
  EntityFilter ->
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
matchesFilter :: EntityFilter -> SomeUpdate ViraState AffectedEntities -> Bool
matchesFilter (FilterRepo repo) update = Entity.affectsRepo repo update
matchesFilter (FilterJob jobId) update = Entity.affectsJob jobId update
matchesFilter FilterAnyJob update = Entity.affectsAnyJob update
