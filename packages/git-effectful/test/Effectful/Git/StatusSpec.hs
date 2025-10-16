module Effectful.Git.StatusSpec where

import Effectful.Git.Command.Status
import Effectful.Git.Types (BranchName (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseGitStatusPorcelain" $ do
    it "parses clean working tree" $ do
      let output =
            "# branch.oid 919259a599a95461dd6fc480a1bce6721bc6e49f\n\
            \# branch.head ci-cli\n\
            \# branch.upstream origin/ci-cli\n\
            \# branch.ab +0 -0\n"
      parseGitStatusPorcelain output `shouldBe` Right (GitStatusPorcelain {branch = BranchName "ci-cli", dirty = False})

    it "parses dirty working tree" $ do
      let output =
            "# branch.oid 919259a599a95461dd6fc480a1bce6721bc6e49f\n\
            \# branch.head ci-cli\n\
            \# branch.upstream origin/ci-cli\n\
            \# branch.ab +0 -0\n\
            \1 .M N... 100644 100644 100644 e326f43ac549794ac28839a68616a5b19255d7fa e326f43ac549794ac28839a68616a5b19255d7fa packages/vira/src/Vira/CI/Pipeline.hs\n"
      parseGitStatusPorcelain output `shouldBe` Right (GitStatusPorcelain {branch = BranchName "ci-cli", dirty = True})

    it "handles missing branch.head" $ do
      let output = "# branch.oid 919259a599a95461dd6fc480a1bce6721bc6e49f\n"
      parseGitStatusPorcelain output `shouldBe` Left "No branch.head found in git status output"

    it "ignores empty lines when checking dirty status" $ do
      let output =
            "# branch.oid 919259a599a95461dd6fc480a1bce6721bc6e49f\n\
            \# branch.head main\n\
            \# branch.upstream origin/main\n\
            \# branch.ab +0 -0\n\
            \\n\
            \\n"
      parseGitStatusPorcelain output `shouldBe` Right (GitStatusPorcelain {branch = BranchName "main", dirty = False})
