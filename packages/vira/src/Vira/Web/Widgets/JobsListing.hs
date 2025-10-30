{-# LANGUAGE OverloadedRecordDot #-}

{- |
Job listing widget component.

Provides a unified, consistent way to display job rows across the application.
-}
module Vira.Web.Widgets.JobsListing (
  viraJobRow_,
  viraJobContextHeader_,
  viraBranchDetailsRow_,
) where

import Data.Time (diffUTCTime)
import Effectful.Git (Commit (..))
import Lucid
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
import Vira.Web.Widgets.Commit qualified as W
import Vira.Web.Widgets.Status qualified as Status
import Vira.Web.Widgets.Time qualified as Time
import Web.TablerIcons.Outline qualified as Icon

{- |
Unified job row widget with optional extra info.

Displays job information in a consistent 12-column grid layout with:
- Job ID with icon
- Commit info (compact)
- Both relative time and duration
- Status badge

The row is wrapped in a simple link to the job page.

= Usage Examples

@
-- Index page: show repo/branch above each job
forM_ jobs $ \job -> do
  let extraInfo = div_ [class_ "text-sm text-gray-600 dark:text-gray-300 mb-1"] $ do
        a_ [href_ repoUrl] $ toHtml job.repo
        span_ " → "
        a_ [href_ branchUrl] $ toHtml job.branch
  viraJobRow_ (Just extraInfo) job

-- Branch page: no extra info
forM_ jobs $ \job ->
  viraJobRow_ Nothing job
@

= Parameters

- extraInfo: Optional HTML content rendered above the job row
- job: The job to display
-}
viraJobRow_ :: Maybe (AppHtml ()) -> St.Job -> AppHtml ()
viraJobRow_ mExtraInfo job = do
  maybeCommit <- lift $ App.query $ St.GetCommitByIdA job.commit
  jobUrl <- lift $ getLinkUrl $ LinkTo.Job job.jobId

  div_ [class_ "space-y-1"] $ do
    -- Extra info rendered outside/above the row
    whenJust mExtraInfo Prelude.id

    -- Job row as simple clickable link
    a_ [href_ jobUrl, class_ "block p-3 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 border border-gray-200 dark:border-gray-700 transition-colors"] $ do
      div_ [class_ "grid grid-cols-12 gap-4 items-center"] $ do
        -- Column 1: Job ID (2 columns)
        div_ [class_ "col-span-2 flex items-center space-x-2"] $ do
          div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-600 dark:text-gray-400"] $ toHtmlRaw Icon.git_commit
          span_ [class_ "text-sm font-semibold text-gray-900 dark:text-gray-100"] $ "#" <> toHtml (show @Text job.jobId)

        -- Column 2: Commit info (4 columns)
        div_ [class_ "col-span-4 min-w-0"] $ do
          W.viraCommitInfoCompact_ maybeCommit

        -- Column 3: Time display - both relative and duration (4 columns)
        div_ [class_ "col-span-4 flex items-center space-x-3 text-xs text-gray-500 dark:text-gray-400"] $ do
          -- Relative time
          div_ [class_ "flex items-center space-x-1"] $ do
            div_ [class_ "w-3 h-3 flex items-center justify-center"] $ toHtmlRaw Icon.clock
            Time.viraRelativeTime_ job.jobCreatedTime

          -- Duration (if job has ended)
          case St.jobEndTime job of
            Just endTime -> do
              let duration = diffUTCTime endTime job.jobCreatedTime
              span_ [class_ "text-gray-400 dark:text-gray-500"] "•"
              Time.viraDuration_ duration
            Nothing -> mempty

        -- Column 4: Status badge (2 columns)
        div_ [class_ "col-span-2 flex justify-end"] $ do
          Status.viraStatusBadge_ job.jobStatus

{- |
Simple context header for job listings.

A text-based heading (no background) that provides context about which
repo/branch the following job rows belong to. Links to the relevant page.

= Usage Examples

@
-- Index page: repo → branch
viraJobContextHeader_ branchUrl $ do
  a_ [href_ repoUrl, class_ "..."] $ toHtml repo
  span_ [class_ "text-gray-400 dark:text-gray-500 mx-2"] " → "
  toHtml branch

-- Repo page: branch with commit info and metadata
viraJobContextHeader_ branchUrl $ do
  div_ [class_ "flex items-center space-x-3"] $ do
    div_ [...] Icon.git_branch
    span_ [class_ "font-semibold"] branchName
    W.viraCommitInfoCompact_ (Just headCommit)
    -- metadata...
@
-}
viraJobContextHeader_ :: Text -> AppHtml () -> AppHtml ()
viraJobContextHeader_ url content = do
  a_
    [ href_ url
    , class_ "group block mb-2 pl-3 text-lg font-bold text-gray-900 dark:text-gray-100 hover:text-indigo-600 dark:hover:text-indigo-400 transition-colors"
    ]
    content

{- | Render a branch details row with optional repo name.

Canonical widget for displaying branch information across the application.
Shows:
- Branch name (with optional repo prefix)
- Out-of-date badge if needed
- Commit info
- Latest job row (if exists)

Used by both IndexPage (with repo name) and RepoPage (without repo name).
-}
viraBranchDetailsRow_ ::
  -- | Show repo name? (True for IndexPage, False for RepoPage)
  Bool ->
  St.BranchDetails ->
  AppHtml ()
viraBranchDetailsRow_ showRepo details = do
  branchUrl <- lift $ getLinkUrl $ LinkTo.RepoBranch details.branch.repoName details.branch.branchName

  -- Smarter badge logic
  let badgeState = case details.mLatestJob of
        Nothing -> Just NeverBuilt
        Just job
          | job.commit /= details.branch.headCommit.id -> Just OutOfDate
          | otherwise -> Nothing

  div_ [class_ "space-y-1"] $ do
    -- Branch header: repo → branch → commit with optional status badge
    viraJobContextHeader_ branchUrl $ do
      div_ [class_ "flex items-center justify-between"] $ do
        -- Left: branch identifier → commit info (natural flow)
        div_ [class_ "flex items-center space-x-2"] $ do
          -- Repo name (if shown)
          when showRepo $ do
            div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-600 dark:text-gray-400"] $ toHtmlRaw Icon.book_2
            span_ [class_ "text-gray-900 dark:text-gray-100"] $ toHtml $ toString details.branch.repoName
            span_ [class_ "mx-2 text-gray-400 dark:text-gray-500"] "→"

          -- Branch name
          div_ [class_ "w-5 h-5 flex items-center justify-center text-gray-600 dark:text-gray-400"] $ toHtmlRaw Icon.git_branch
          span_ [class_ "text-gray-900 dark:text-gray-100"] $ toHtml $ toString details.branch.branchName

          -- Commit info right next to branch
          span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
          div_ [class_ "text-xs text-gray-600 dark:text-gray-400"] $ do
            W.viraCommitInfoCompact_ (Just details.branch.headCommit)

        -- Right: status badge + build count (only when no job)
        div_ [class_ "flex items-center space-x-2"] $ do
          -- Build count only shown when no job exists
          when (isNothing details.mLatestJob && details.jobsCount > 0) $ do
            span_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
              "(" <> toHtml (show @Text details.jobsCount) <> " builds)"

          -- Status badge
          whenJust badgeState $ \case
            NeverBuilt ->
              span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300"] $ do
                div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.alert_circle
                "Never built"
            OutOfDate ->
              span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300"] $ do
                div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.clock
                "Out of date"

    -- Job row - only if job exists
    whenJust details.mLatestJob $ \latestJob -> do
      div_ [class_ "ml-7"] $ do
        viraJobRow_ Nothing latestJob

-- | Badge state for branch
data BadgeState = NeverBuilt | OutOfDate
