{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time status of the Vira system.
module Vira.Stream.Status (
  -- * Routes and handlers
  streamRouteHandler,

  -- * Views
  viewStream,
  indicator,
) where

import Control.Concurrent (threadDelay)
import Effectful (Eff)
import Lucid
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.HTMX.SSE qualified as SSE
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Stack (AppStack)
import Vira.State.Acid qualified as Acid
import Vira.State.Type
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader, ask, asks, runReader)

viewStream :: AppHtml ()
viewStream = do
  SSE.sseConnect LinkTo.StatusGet $ do
    SSE.sseSwap "status" view

streamRouteHandler :: SourceT (Eff AppStack) SSE.SSEMessage
streamRouteHandler = S.fromStepT $ step 0
  where
    step (n :: Int) = S.Effect $ do
      when (n > 0) $ do
        -- We must use polling only because there's no proper update notification mechanism yet.
        liftIO $ threadDelay 1_000_000
      html <- App.runAppHtmlHandlingError view
      let msg = SSE.SSEMessage "status" (show n) html
      pure $ S.Yield msg $ step (n + 1)

-- | Status view for both immediate display and SSE streaming
view :: AppHtml ()
view = do
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
