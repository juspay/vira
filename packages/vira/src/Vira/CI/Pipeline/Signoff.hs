module Vira.CI.Pipeline.Signoff (
  performSignoff,
) where

import BB.CLI.Invocation qualified as BBInvocation
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, catchError, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Git.Platform (GitPlatform (..))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import GH.Signoff qualified as GHSignoff
import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect (PipelineEnv, logPipeline)
import Vira.CI.Pipeline.Process (runProcess)
import Vira.Environment.Tool.Tools.Bitbucket (mkBitbucketSuggestion)
import Vira.Environment.Tool.Tools.Bitbucket.CLI (bbBin)

{- | Create commit signoffs for a specific git platform.

Takes the platform type, repository directory, signoff names, and output log.
Handles platform-specific signoff process creation and error handling.
-}
performSignoff ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  -- | Git platform (GitHub or Bitbucket)
  GitPlatform ->
  -- | Repository directory
  FilePath ->
  -- | Signoff names (e.g., "vira/x86_64-linux")
  NonEmpty String ->
  -- | Output log destination
  Maybe FilePath ->
  Eff es ()
performSignoff platform repoDir signoffNames outputLog = do
  case platform of
    GitHub -> do
      logPipeline Info $ "Creating " <> show (length signoffNames) <> " GitHub signoffs: " <> show (toList signoffNames)
      let ghProc = GHSignoff.create GHSignoff.Force signoffNames
      runProcess repoDir outputLog ghProc
      logPipeline Info "All GitHub signoffs succeeded"
    Bitbucket bitbucketHost -> do
      logPipeline Info $ "Creating " <> show (length signoffNames) <> " Bitbucket signoffs: " <> show (toList signoffNames)
      let bbProcs = BBInvocation.createSignoff bbBin BBInvocation.Force signoffNames
          bitbucketUrl = "https://" <> bitbucketHost
          suggestion = mkBitbucketSuggestion bitbucketUrl
          handler _callstack err = do
            logPipeline Error $ "Bitbucket signoff failed: " <> show err
            logPipeline Info $ "If authentication is required, run: " <> show @Text suggestion
            throwError err
      -- Run each signoff process sequentially
      forM_ bbProcs $ \bbProc ->
        runProcess repoDir outputLog bbProc `catchError` handler
      logPipeline Info "All Bitbucket signoffs succeeded"
