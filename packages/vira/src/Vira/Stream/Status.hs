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
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (VHtml, getLinkUrl, runVHtml')
import Vira.App.Stack (AppStack)
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

viewStream :: VHtml ()
viewStream = do
  link <- lift $ getLinkUrl LinkTo.StatusGet
  div_ [hxExt_ "sse", hxSseConnect_ link, hxSseSwap_ "status"] $ do
    viewInner

-- | Status view for both immediate display and SSE streaming
viewInner :: VHtml ()
viewInner = do
  -- Compute running jobs directly
  jobsData <- lift $ App.query Acid.GetRunningJobs
  let jobs = jobsData <&> \job -> (job.jobRepo, job.jobId)
  div_ [class_ "flex items-center space-x-2", title_ "Build Status"] $ do
    indicator $ not $ null jobs
    forM_ jobs $ \(repo, jobId) -> do
      jobUrl <- lift $ getLinkUrl $ LinkTo.Job jobId
      a_ [href_ jobUrl] $ do
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

streamRouteHandler :: SourceT (Eff AppStack) Status
streamRouteHandler = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        liftIO $ threadDelay 1_000_000
      html <- runVHtml' viewInner
      let msg = Status n html
      pure $ S.Yield msg $ step (n + 1)
