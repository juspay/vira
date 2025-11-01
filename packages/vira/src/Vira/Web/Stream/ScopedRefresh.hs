{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Entity-scoped Server-Sent Events for automatic page refresh.

Uses the event bus to subscribe to relevant updates for the current page,
triggers page reload when matching events occur.

Flow: breadcrumbs → 'pageFilterKey' → 'viewStreamScoped' → SSE → 'streamRouteHandler' → reload
-}
module Vira.Web.Stream.ScopedRefresh (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStreamScoped,
) where

import Colog.Core (Severity (Debug))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM qualified as STM
import Data.Acid.Events (SomeUpdate (..), matchUpdate)
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Colog.Simple (log)
import Effectful.Concurrent (threadDelay)
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
import Vira.State.Type (Job (jobId), Repo (name))
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Stack (tagStreamThread)
import Vira.Web.Stream.KeepAlive (KeepAlive)
import Vira.Web.Stream.KeepAlive qualified as KeepAlive
import Prelude hiding (Reader, ask, asks, filter, runReader)

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

-- | Add SSE listener for auto-refresh based on page breadcrumbs
viewStreamScoped :: [LinkTo] -> AppHtml ()
viewStreamScoped crumbs =
  whenJust (pageFilterKey crumbs) $ \filterKey -> do
    link <- lift $ getLinkUrl $ LinkTo.Refresh (Just filterKey)
    -- Use native EventSource for reliable refresh-triggered reloads
    script_ $
      "new EventSource('" <> link <> "').addEventListener('refresh', () => location.reload());"

-- | Derive filter key from breadcrumbs (internal)
pageFilterKey :: [LinkTo] -> Maybe Text
pageFilterKey crumbs = case reverse crumbs of
  (Job jobId : _) -> Just $ "job:" <> show jobId
  (RepoBranch repoName _ : _) -> Just $ "repo:" <> toText repoName
  (Repo repoName : _) -> Just $ "repo:" <> toText repoName
  [] -> Just "index" -- Index page: subscribe to all job events
  _ -> Nothing -- No refresh for other pages

data StreamConfig = StreamConfig
  { counter :: Int
  , predicate :: SomeUpdate ViraState -> Bool
  , channel :: STM.TChan (SomeUpdate ViraState)
  }

streamRouteHandler :: (HasCallStack) => Maybe Text -> SourceT (Eff AppStack) (KeepAlive ScopedRefresh)
streamRouteHandler mFilterKey = S.fromStepT $ S.Effect $ do
  tagStreamThread
  let filterKey = fromMaybe "index" mFilterKey
  let predicate = buildPredicate filterKey
  log Debug $ "Starting stream with filter: " <> filterKey
  chan <- App.subscribe
  pure $ step StreamConfig {counter = 0, predicate, channel = chan}
  where
    step cfg =
      KeepAlive.withKeepAlive
        (waitForRelevantUpdate cfg.channel cfg.predicate)
        ( \matchingUpdates -> do
            log Debug $ "Sending refresh for " <> show (length matchingUpdates) <> " matching events; n=" <> show cfg.counter
            pure $ Just (ScopedRefresh, cfg {counter = cfg.counter + 1})
        )
        cfg
        step

-- | Wait for and collect relevant updates (with debouncing)
waitForRelevantUpdate ::
  STM.TChan (SomeUpdate ViraState) ->
  (SomeUpdate ViraState -> Bool) ->
  Eff AppStack (NonEmpty (SomeUpdate ViraState))
waitForRelevantUpdate chan predicate = do
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

-- | Build a predicate function from a filter key
buildPredicate :: Text -> (SomeUpdate ViraState -> Bool)
buildPredicate key
  | "job:" `T.isPrefixOf` key =
      let jobIdText = T.drop 4 key
       in matchesJobText jobIdText
  | "repo:" `T.isPrefixOf` key =
      let repoText = T.drop 5 key
       in matchesRepoText repoText
  | key == "index" = matchesAnyJob
  | otherwise = const False

-- | Check if update affects a specific job (by text representation)
matchesJobText :: Text -> SomeUpdate ViraState -> Bool
matchesJobText jobIdText update =
  case matchUpdate @JobUpdateStatusA update of
    Just (JobUpdateStatusA jid _, _) -> show jid == jobIdText
    Nothing -> case matchUpdate @AddNewJobA update of
      Just (AddNewJobA {}, job) -> show job.jobId == jobIdText
      Nothing -> False

-- | Check if update affects a specific repo (by text name)
matchesRepoText :: Text -> SomeUpdate ViraState -> Bool
matchesRepoText repoText update =
  case matchUpdate @SetAllReposA update of
    Just (SetAllReposA repos, _) -> any (\r -> toText r.name == repoText) repos
    Nothing -> case matchUpdate @AddNewRepoA update of
      Just (AddNewRepoA r, _) -> toText r.name == repoText
      Nothing -> case matchUpdate @DeleteRepoByNameA update of
        Just (DeleteRepoByNameA name, Right ()) -> toText name == repoText
        _ -> case matchUpdate @SetRepoA update of
          Just (SetRepoA r, _) -> toText r.name == repoText
          Nothing -> case matchUpdate @SetRepoBranchesA update of
            Just (SetRepoBranchesA name _, _) -> toText name == repoText
            Nothing -> case matchUpdate @AddNewJobA update of
              Just (AddNewJobA repo _ _ _ _, _) -> toText repo == repoText
              Nothing -> False

-- | Check if update is any job-related event
matchesAnyJob :: SomeUpdate ViraState -> Bool
matchesAnyJob update =
  isJust (matchUpdate @JobUpdateStatusA update)
    || isJust (matchUpdate @AddNewJobA update)
