-- | `System.Process` utilities
module Vira.Lib.Process (
  alwaysUnderPath,
  redirectOutputTo,
) where

import Effectful.Process (
  CreateProcess (cwd, std_err, std_out),
  StdStream (UseHandle),
 )
import System.FilePath ((</>))

-- | Make sure that this process will always use the given path, or its subdirectory, as CWD. If the current CWD is relative, it will be made relative to the given path.
alwaysUnderPath :: FilePath -> CreateProcess -> CreateProcess
alwaysUnderPath path p =
  p
    { cwd = Just $ maybe path (path </>) (cwd p)
    }

-- | With stdout and stderr redirected to given handle
redirectOutputTo :: Handle -> CreateProcess -> CreateProcess
redirectOutputTo h p =
  p
    { std_out = UseHandle h
    , std_err = UseHandle h
    }
