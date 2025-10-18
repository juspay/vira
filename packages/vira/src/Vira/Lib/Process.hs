-- | `System.Process` utilities
module Vira.Lib.Process (
  redirectOutputTo,
) where

import Effectful.Process (
  CreateProcess (std_err, std_out),
  StdStream (UseHandle),
 )

-- | With stdout and stderr redirected to given handle
redirectOutputTo :: Handle -> CreateProcess -> CreateProcess
redirectOutputTo h p =
  p
    { std_out = UseHandle h
    , std_err = UseHandle h
    }
