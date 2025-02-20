{-# LANGUAGE OverloadedRecordDot #-}

-- | Real-time status of the Vira system.
module Vira.Status where

import Control.Concurrent (threadDelay)
import Effectful (Eff)
import Htmx.Lucid.Extra (hxExt_)
import Lucid
import Servant (Link, linkURI)
import Servant.API.EventStream
import Servant.Types.SourceT qualified as S
import Vira.App qualified as App
import Vira.App.LinkTo (LinkTo)
import Vira.App.LinkTo qualified as LinkTo
import Vira.Lib.HTMX
import Vira.State.Acid qualified as Acid
import Vira.State.Type
import Prelude hiding (Reader, ask, asks, runReader)

data Status = Status Int (Html ())

instance ToServerEvent Status where
  toServerEvent (Status ident t) =
    ServerEvent
      (Just "status")
      (Just $ show ident)
      (Lucid.renderBS t)

handler :: App.AppState -> S.SourceT IO Status
handler cfg = S.fromStepT $ step 0
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

view :: Html ()
view = do
  div_ [hxExt_ "sse", hxSseConnect_ "/status", hxSseSwap_ "status"] $ do
    "Loading status..."

viewInner :: (LinkTo -> Link) -> [(RepoName, JobId)] -> Html ()
viewInner linkTo jobs = do
  div_ [class_ "flex items-center space-x-2", title_ "Build Status"] $ do
    indicator $ not $ null jobs
    forM_ jobs $ \(repo, jobId) -> do
      a_ [href_ $ show . linkURI $ linkTo $ LinkTo.Job jobId] $ do
        span_ $ b_ $ toHtml $ unRepoName repo
        "/"
        span_ $ code_ $ toHtml @Text $ show jobId

indicator :: Bool -> Html ()
indicator active = do
  let classes = if not active then "border-blue-300" else "border-blue-500 animate-ping"
  div_ [class_ $ "w-4 h-4 border-2 rounded-full " <> classes] ""
