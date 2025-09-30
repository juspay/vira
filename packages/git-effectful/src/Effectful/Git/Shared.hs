{- | Shared git clone management with file-based locking.

Maintains a single clone per repository, reusing it across operations to avoid
repeated cloning. Uses file locks to serialize concurrent updates to the same
repository, working across processes.

= Usage

@
result <- ensureAndUpdateSharedClone "https://..." "/path/to/clone"
case result of
  Right () -> -- clone ready for use
  Left err -> -- handle error, may need to delete corrupt clone
@
-}
module Effectful.Git.Shared (
  ensureAndUpdateSharedClone,
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

{- | Ensure shared clone exists and is up-to-date.

Clones if needed, then fetches latest.
Concurrent calls for the same directory block on file lock until the first completes.
On error, caller may need to delete the clone directory and retry.
-}
ensureAndUpdateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  -- | Clone URL
  Text ->
  -- | Shared clone directory path
  FilePath ->
  Eff es (Either Text ())
ensureAndUpdateSharedClone cloneUrl sharedClonePath = do
  ensureResult <- ensureSharedClone cloneUrl sharedClonePath
  case ensureResult of
    Left err -> return $ Left err
    Right () -> updateSharedClone sharedClonePath

{- | Clone repository if not present. Idempotent.

Checks directory existence first. If missing, acquires lock and clones.
Race condition: Multiple processes may see "doesn't exist" before first clone completes,
but file lock ensures only one actually clones.
-}
ensureSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  Text ->
  FilePath ->
  Eff es (Either Text ())
ensureSharedClone cloneUrl sharedClonePath = do
  -- Check if shared clone directory exists
  exists <- liftIO $ doesDirectoryExist sharedClonePath

  if exists
    then do
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Shared clone already exists: " <> toText sharedClonePath
          , msgStack = callStack
          }
      return $ Right ()
    else do
      -- Clone the repository with file lock protection
      Log.logMsg $
        Msg
          { msgSeverity = Info
          , msgText = "Creating shared clone at " <> toText sharedClonePath
          , msgStack = callStack
          }

      withFileLock sharedClonePath $ do
        -- Create parent directory
        liftIO $ createDirectoryIfMissing True (takeDirectory sharedClonePath)

        -- Clone the repository
        let cloneCmd = cloneAllBranches cloneUrl (takeFileName sharedClonePath)
            parentDir = takeDirectory sharedClonePath

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
                , msgText = "Successfully created shared clone at " <> toText sharedClonePath
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
                "Failed to create shared clone at "
                  <> toText sharedClonePath
                  <> ". Please delete it and try again."

{- | Fetch latest changes from remote. Uses @--force@ to handle force-pushes.

Acquires file lock before fetching to prevent concurrent updates to same repo.
-}
updateSharedClone ::
  ( Log Message :> es
  , IOE :> es
  ) =>
  FilePath ->
  Eff es (Either Text ())
updateSharedClone sharedClonePath = do
  Log.logMsg $
    Msg
      { msgSeverity = Info
      , msgText = "Updating shared clone at " <> toText sharedClonePath
      , msgStack = callStack
      }

  withFileLock sharedClonePath $ do
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
          fetchCmd {cwd = Just sharedClonePath}
          ""

    case exitCode of
      ExitSuccess -> do
        Log.logMsg $
          Msg
            { msgSeverity = Info
            , msgText = "Successfully updated shared clone at " <> toText sharedClonePath
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
            "Failed to update shared clone at "
              <> toText sharedClonePath
              <> ". Please delete it and try again."

{- | Run action with file-based lock on a directory.

Creates/acquires @dirPath.lock@ file.
Blocks if another process holds the lock, ensuring sequential access.
Exception-safe via onException.
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
