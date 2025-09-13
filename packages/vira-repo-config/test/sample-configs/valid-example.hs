-- | Sample valid configuration file
module Config where

import Data.Text qualified as T
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (ViraPipeline (..), BuildStage (..), AtticStage (..), CachixStage (..), SignoffStage (..))
import Optics.Core

-- | Required function that configures the CI pipeline
configureVira :: ViraEnvironment -> ViraPipeline -> ViraPipeline
configureVira env pipeline =
  let isMainBranch = env.branch.branchName == "main"
      isReleaseBranch = "release-" `T.isPrefixOf` (toText env.branch.branchName)
      isPullRequest = "pr-" `T.isPrefixOf` (toText env.branch.branchName)
  in pipeline
     -- Enable attic for main and release branches
     & #attic % #atticEnable .~ (isMainBranch || isReleaseBranch)
     -- Enable cachix for all branches except PRs
     & #cachix % #cachixEnable .~ not isPullRequest
     -- Add override inputs for release branches
     & #build % #overrideInputs .~ [("local", "false") | isReleaseBranch]
     -- Enable signoff for main branch
     & #signoff % #signoffEnable .~ isMainBranch