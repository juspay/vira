{- | Git config operations

Provides git config commands for reading repository and user configuration.
-}
module Effectful.Git.Command.Config (
  getConfig,
  getUserName,
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

{- | Get a git config value

Gets the value for the specified config key in the given repository directory.
-}
getConfig ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  -- | Config key (e.g., "user.name", "user.email")
  Text ->
  Eff es Text
getConfig repoDir key = do
  let cmd = proc git ["-C", repoDir, "config", toString key]
  output <- withLogCommand cmd $ do
    log Debug $ "Running git config " <> key
    readCreateProcess cmd ""
  pure $ T.strip $ toText output

{- | Get the git user name

Convenience function to get user.name from git config.
-}
getUserName ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  -- | Repository directory
  FilePath ->
  Eff es Text
getUserName repoDir = getConfig repoDir "user.name"
