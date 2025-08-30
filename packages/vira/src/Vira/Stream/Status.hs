{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time status of the Vira system.
module Vira.Stream.Status (
  -- * Routes and handlers
  StreamRoute,
  streamRouteHandler,

  -- * Views
  viewStream,
  indicator,
) where

import Control.Concurrent (threadDelay)
import Effectful (Eff)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Lucid.Htmx.Contrib (hxSseConnect_, hxSseSwap_)
import Servant.API
import Servant.API.EventStream
import Servant.Links (linkURI)
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo.Type (LinkTo)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.State.Acid qualified as Acid
import Vira.State.Type
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, asks, runReader)

type StreamRoute = ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Status))

-- A status message sent from server to client
--
-- The `Int` is the unique identifier of the status message, which contains the
-- raw HTML of the status.
data Status = Status Int (Html ())

instance ToServerEvent Status where
  toServerEvent (Status ident t) =
    ServerEvent
      (Just "status")
      (Just $ show ident)
      (Lucid.renderBS t)

viewStream :: (Monad m) => (LinkTo.LinkTo -> Link) -> HtmlT m ()
viewStream linkTo = do
  div_ [hxExt_ "sse", hxSseConnect_ link, hxSseSwap_ "status"] $ do
    "Loading status..."
  where
    link = show . linkURI $ linkTo LinkTo.StatusGet

viewInner :: (LinkTo -> Link) -> [(RepoName, JobId)] -> Html ()
viewInner linkTo jobs = do
  div_ [class_ "flex items-center space-x-2", title_ "Build Status"] $ do
    indicator $ not $ null jobs
    forM_ jobs $ \(repo, jobId) -> do
      a_ [href_ $ show . linkURI $ linkTo $ LinkTo.Job jobId] $ do
        span_ $ b_ $ toHtml $ unRepoName repo
        "/"
        span_ $ code_ $ toHtml @Text $ show jobId

indicator :: (Monad m) => Bool -> HtmlT m ()
indicator active = do
  let (iconSvg, classes) =
        if active
          then (Icon.loader_2, "text-green-500 animate-spin")
          else (Icon.circle, "text-gray-500")
  div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
    toHtmlRaw iconSvg

streamRouteHandler :: App.AppState -> SourceIO Status
streamRouteHandler cfg = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        liftIO $ threadDelay 1_000_000
      jobs <- App.runApp cfg runningJobs
      let msg = Status n $ viewInner cfg.linkTo jobs
      pure $ S.Yield msg $ step (n + 1)

runningJobs :: Eff App.AppStack [(RepoName, JobId)]
runningJobs = do
  jobs <- App.query Acid.GetRunningJobs
  pure $
    jobs <&> \job ->
      (job.jobRepo, job.jobId)
