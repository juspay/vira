{- | Application foundation for Vira to build the logic on top of.

Everything under `Vira.App.*` can be considered devoid of application logic.
-}
module Vira.App (
  module X,
)
where

import Vira.App.AcidState as X
import Vira.App.CLI as X
import Vira.App.Logging as X (log)
import Vira.App.Servant as X
import Vira.App.Stack as X
