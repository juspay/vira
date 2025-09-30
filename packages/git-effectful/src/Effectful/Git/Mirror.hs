{- | Git repository mirroring with file-based locking.

Maintains a synchronized mirror of a git repository (metadata only, without blobs)
that can be safely accessed and updated by multiple processes. Uses file locks
to serialize concurrent updates to the same mirror.

The mirror is created with @--filter=blob:none@ for efficiency, containing only
repository metadata (refs, commits, trees) without file blobs.

= Usage

@
result <- syncMirror "https://..." "/path/to/mirror"
case result of
  Right () -> -- mirror is up-to-date and ready for use
  Left err -> -- handle error, may need to delete corrupt mirror
@
-}
module Effectful.Git.Mirror (
  syncMirror,
) where

import Colog (Message, Msg (..), Severity (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog qualified as Log
import Effectful.Git (git)
import Lukko (LockMode (ExclusiveLock))
import Lukko qualified
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import UnliftIO.Exception (finally)

{- | Ensure git mirror exists and is up-to-date.

Creates mirror if needed, then fetches latest changes.
Concurrent calls for the same directory block on file lock until the first completes.
On error, caller may need to delete the mirror directory and retry.
-}
syncMirror ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  -- | Clone URL
  Text ->
  -- | Mirror directory path
  FilePath ->
  Eff es (Either Text ())
syncMirror cloneUrl mirrorPath = do
  ensureResult <- ensureMirror cloneUrl mirrorPath
  case ensureResult of
    Left err -> return $ Left err
    Right () -> updateMirror mirrorPath

{- | Clone repository mirror if not present. Idempotent.

Checks directory existence first. If missing, acquires lock and clones.
Race condition: Multiple processes may see "doesn't exist" before first clone completes,
but file lock ensures only one actually clones.
-}
ensureMirror ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  Text ->
  FilePath ->
  Eff es (Either Text ())
ensureMirror cloneUrl mirrorPath = do
  -- Check if mirror directory exists
  exists <- liftIO $ doesDirectoryExist mirrorPath

  if exists
    then do
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Git mirror already exists: " <> toText mirrorPath
          , msgStack = callStack
          }
      return $ Right ()
    else do
      -- Clone the repository with file lock protection
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Creating git mirror at " <> toText mirrorPath
          , msgStack = callStack
          }

      withFileLock mirrorPath $ do
        -- Create parent directory
        liftIO $ createDirectoryIfMissing True (takeDirectory mirrorPath)

        -- Clone the repository
        let cloneCmd = cloneAllBranches cloneUrl (takeFileName mirrorPath)
            parentDir = takeDirectory mirrorPath

        Log.logMsg $
          Msg
            { msgSeverity = Info
            , msgText = "Running git clone: " <> show (cmdspec cloneCmd)
            , msgStack = callStack
            }

        (exitCode, stdoutStr, stderrStr) <-
          liftIO $
            readCreateProcessWithExitCode
              cloneCmd {cwd = Just parentDir}
              ""

        case exitCode of
          ExitSuccess -> do
            Log.logMsg $
              Msg
                { msgSeverity = Info
                , msgText = "Successfully created git mirror at " <> toText mirrorPath
                , msgStack = callStack
                }
            return $ Right ()
          ExitFailure code -> do
            let errorMsg =
                  "Git clone failed with exit code "
                    <> show code
                    <> ". Stdout: "
                    <> toText stdoutStr
                    <> ". Stderr: "
                    <> toText stderrStr
            Log.logMsg $
              Msg
                { msgSeverity = Error
                , msgText = errorMsg
                , msgStack = callStack
                }
            return $
              Left $
                "Failed to create git mirror at "
                  <> toText mirrorPath
                  <> ". Please delete it and try again."

{- | Fetch latest changes from remote. Uses @--force@ to handle force-pushes.

Acquires file lock before fetching to prevent concurrent updates to same mirror.
-}
updateMirror ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  FilePath ->
  Eff es (Either Text ())
updateMirror mirrorPath = do
  Log.logMsg $
    Msg
      { msgSeverity = Info
      , msgText = "Updating git mirror at " <> toText mirrorPath
      , msgStack = callStack
      }

  withFileLock mirrorPath $ do
    -- Use --force to handle forced pushes
    let fetchCmd = fetchAllBranches

    Log.logMsg $
      Msg
        { msgSeverity = Info
        , msgText = "Running git fetch: " <> show (cmdspec fetchCmd)
        , msgStack = callStack
        }

    (exitCode, stdoutStr, stderrStr) <-
      liftIO $
        readCreateProcessWithExitCode
          fetchCmd {cwd = Just mirrorPath}
          ""

    case exitCode of
      ExitSuccess -> do
        Log.logMsg $
          Msg
            { msgSeverity = Info
            , msgText = "Successfully updated git mirror at " <> toText mirrorPath
            , msgStack = callStack
            }
        return $ Right ()
      ExitFailure code -> do
        let errorMsg =
              "Git fetch failed with exit code "
                <> show code
                <> ". Stdout: "
                <> toText stdoutStr
                <> ". Stderr: "
                <> toText stderrStr
        Log.logMsg $
          Msg
            { msgSeverity = Error
            , msgText = errorMsg
            , msgStack = callStack
            }
        return $
          Left $
            "Failed to update git mirror at "
              <> toText mirrorPath
              <> ". Please delete it and try again."

{- | Run action with file-based lock on a directory.

Creates/acquires @dirPath.lock@ file.
Blocks if another process holds the lock, ensuring sequential access.
Exception-safe via finally.
-}
withFileLock ::
  (IOE :> es) =>
  -- | Directory to lock
  FilePath ->
  Eff es a ->
  Eff es a
withFileLock dirPath action = do
  let lockPath = dirPath <> ".lock"

  -- Acquire lock
  fd <- liftIO $ do
    createDirectoryIfMissing True (takeDirectory lockPath)
    fd <- Lukko.fdOpen lockPath
    Lukko.fdLock fd ExclusiveLock
    return fd

  -- Run action with cleanup via finally
  action `finally` liftIO (Lukko.fdUnlock fd >> Lukko.fdClose fd)

-- | Return the `CreateProcess` to clone a repo with all branches (blob:none for efficiency)
cloneAllBranches :: Text -> FilePath -> CreateProcess
cloneAllBranches url path =
  proc
    git
    [ "clone"
    , "-v"
    , "--filter=blob:none"
    , "--no-single-branch"
    , toString url
    , path
    ]

-- | Return the `CreateProcess` to fetch all branches with force
fetchAllBranches :: CreateProcess
fetchAllBranches =
  proc
    git
    [ "fetch"
    , "--force"
    , "origin"
    , "+refs/heads/*:refs/remotes/origin/*"
    ]
