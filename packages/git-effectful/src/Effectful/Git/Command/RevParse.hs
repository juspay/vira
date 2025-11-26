{- | Git rev-parse operations

Provides git rev-parse commands for resolving Git object names and references.
-}
module Effectful.Git.Command.RevParse (
  revParse,
  getCurrentCommit,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error)
import Effectful.Git.Core (git)
import Effectful.Process (Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER

{- | Run git rev-parse

Resolves a Git reference to its commit hash or performs other rev-parse operations.
-}
revParse ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  -- | Reference to parse (e.g., "HEAD", "main", "HEAD~1")
  Text ->
  Eff es Text
revParse repoDir ref = do
  let cmd = proc git ["-C", repoDir, "rev-parse", toString ref]
  output <- withLogCommand cmd $ do
    log Debug $ "Running git rev-parse " <> ref
    readCreateProcess cmd ""
  pure $ T.strip $ toText output

{- | Get current commit hash

Convenience function to get the full commit hash of HEAD.
-}
getCurrentCommit ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  Eff es Text
getCurrentCommit repoDir = revParse repoDir "HEAD"
