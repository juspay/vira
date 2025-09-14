-- | Sample valid configuration file
module Config where

import Data.Text qualified as T
import Data.Text (Text)
import Relude (toText)
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (ViraPipeline (..), BuildStage (..), AtticStage (..), CachixStage (..), SignoffStage (..))
import Vira.State.Type (Branch (..))
import Effectful.Git (BranchName (..))
import Optics.Core

-- | Required function that configures the CI pipeline
configureVira :: ViraEnvironment -> ViraPipeline -> ViraPipeline
configureVira env pipeline =
  let isMainBranch = env.branch.branchName == BranchName "main"
      isReleaseBranch = "release-" `T.isPrefixOf` unBranchName env.branch.branchName
      isPullRequest = "pr-" `T.isPrefixOf` unBranchName env.branch.branchName
  in pipeline
     -- Enable attic for main and release branches
     & #attic % #atticEnable .~ (isMainBranch || isReleaseBranch)
     -- Enable cachix for all branches except PRs
     & #cachix % #cachixEnable .~ not isPullRequest
     -- Add override inputs for release branches
     & #build % #overrideInputs .~ [("local", "false") | isReleaseBranch]
     -- Enable signoff for main branch
     & #signoff % #signoffEnable .~ isMainBranch