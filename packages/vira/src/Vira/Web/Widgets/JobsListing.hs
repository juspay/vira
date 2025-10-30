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

import Data.Text qualified as T
import Data.Time (diffUTCTime)
import Effectful.Git (Commit (..))
import Htmx.Lucid.Core (hxSwapS_)
import Htmx.Swap (Swap (..))
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_)
import Vira.App qualified as App
import Vira.State.Acid qualified as St
import Vira.State.Type (BadgeState (..))
import Vira.State.Type qualified as St
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLink, getLinkUrl)
import Vira.Web.Widgets.Button qualified as W
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

  div_ [class_ "mb-6"] $ do
    -- Extra info rendered outside/above the row
    whenJust mExtraInfo Prelude.id

    -- Job row as clickable link with consistent styling
    a_ [href_ jobUrl, class_ "block p-4 rounded-lg bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 border-2 border-gray-200 dark:border-gray-700 transition-all cursor-pointer"] $ do
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
  buildLink <- lift $ getLink $ LinkTo.Build details.branch.repoName details.branch.branchName

  -- Determine where clicking the row should go
  rowUrl <- case details.mLatestJob of
    Just job -> lift $ getLinkUrl $ LinkTo.Job job.jobId
    Nothing -> pure branchUrl

  -- Single unified row with responsive grid and subtle gradient background
  div_ [class_ "relative mb-6"] $ do
    -- Tags - connected segments with color distinction, left-aligned
    div_ [class_ "absolute -top-3 left-3 flex items-center z-10"] $ do
      -- Repo tag (if shown) - purple theme, flat right edge
      when showRepo $ do
        repoUrl <- lift $ getLinkUrl $ LinkTo.Repo details.branch.repoName
        a_ [href_ repoUrl, class_ "flex items-center gap-1 px-3 py-1 bg-purple-100 dark:bg-purple-900 border border-purple-300 dark:border-purple-700 rounded-l-full border-r-0 shadow-sm hover:opacity-70 transition-opacity"] $ do
          div_ [class_ "w-4 h-4 flex items-center justify-center text-purple-700 dark:text-purple-200 shrink-0"] $ toHtmlRaw Icon.book_2
          span_ [class_ "text-sm font-semibold text-purple-900 dark:text-purple-100"] $ toHtml $ toString details.branch.repoName

      -- Branch tag - blue theme (or red if deleted), flat left edge when repo shown
      let branchClasses =
            if details.branch.deleted
              then "flex items-center gap-1 px-3 py-1 bg-red-100 dark:bg-red-900 border border-red-300 dark:border-red-700 shadow-sm hover:opacity-70 transition-opacity"
              else "flex items-center gap-1 px-3 py-1 bg-blue-100 dark:bg-blue-900 border border-blue-300 dark:border-blue-700 shadow-sm hover:opacity-70 transition-opacity"
          branchClasses' = branchClasses <> if showRepo then " rounded-r-full border-l-0" else " rounded-full"
          branchIconColor =
            if details.branch.deleted
              then "w-4 h-4 flex items-center justify-center text-red-700 dark:text-red-200"
              else "w-4 h-4 flex items-center justify-center text-blue-700 dark:text-blue-200"
          branchTextColor =
            if details.branch.deleted
              then "text-sm font-semibold text-red-900 dark:text-red-100"
              else "text-sm font-semibold text-blue-900 dark:text-blue-100"
      a_ [href_ branchUrl, class_ branchClasses'] $ do
        div_ [class_ branchIconColor] $ toHtmlRaw Icon.git_branch
        span_ [class_ branchTextColor] $ toHtml $ toString details.branch.branchName
        when details.branch.deleted $
          span_ [class_ "text-xs text-red-700 dark:text-red-300 ml-1"] "(deleted)"

    -- Main row - clickable, single line layout with gray background and border
    a_
      [ href_ rowUrl
      , class_ "block pt-6 pb-4 px-4 rounded-lg bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 border-2 border-gray-200 dark:border-gray-700 transition-all cursor-pointer"
      ]
      $ do
        div_ [class_ "grid grid-cols-1 lg:grid-cols-12 gap-3 items-center"] $ do
          -- LEFT SECTION: Commit details (8 cols on desktop)
          div_ [class_ "lg:col-span-8 flex items-center gap-2 flex-wrap text-sm"] $ do
            W.viraCommitHash_ details.branch.headCommit.id

            -- Commit message (truncated with tooltip)
            unless (T.null details.branch.headCommit.message) $ do
              span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
              span_
                [ class_ "text-gray-700 dark:text-gray-300 truncate max-w-md"
                , title_ details.branch.headCommit.message
                ]
                $ toHtml details.branch.headCommit.message

            -- Commit time
            span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
            div_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
              Time.viraRelativeTime_ details.branch.headCommit.date

          -- RIGHT SECTION: Job status OR Build button (4 cols on desktop, vertically centered)
          div_ [class_ "lg:col-span-4 flex items-center justify-start lg:justify-end gap-2 flex-wrap"] $ do
            case details.mLatestJob of
              -- Has job and it's current: show job info
              Just job | isNothing details.badgeState -> do
                span_ [class_ "text-sm text-gray-600 dark:text-gray-400"] $ "#" <> toHtml (show @Text job.jobId)
                span_ [class_ "text-gray-400 dark:text-gray-500"] "·"
                case St.jobEndTime job of
                  Just endTime -> do
                    let duration = diffUTCTime endTime job.jobCreatedTime
                    Time.viraDuration_ duration
                  Nothing -> mempty
                Status.viraStatusBadge_ job.jobStatus

              -- Has badge (never built or out of date): show badge + Build button
              _ -> do
                whenJust details.badgeState $ \case
                  NeverBuilt ->
                    span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300"] $ do
                      div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.alert_circle
                      "Never built"
                  OutOfDate ->
                    span_ [class_ "inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300"] $ do
                      div_ [class_ "w-3 h-3 mr-1 flex items-center justify-center"] $ toHtmlRaw Icon.clock
                      "Out of date"

                -- Build button (prevent link propagation) - smaller size for inline row
                W.viraButton_
                  W.ButtonSuccess
                  [ hxPostSafe_ buildLink
                  , hxSwapS_ AfterEnd
                  , onclick_ "event.preventDefault(); event.stopPropagation();"
                  , class_ "!px-3 !py-1.5 !text-xs"
                  ]
                  $ do
                    W.viraButtonIcon_ $ toHtmlRaw Icon.player_play
                    "Build"
