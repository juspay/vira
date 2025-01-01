-- To keep things simple in the beginning, we just use hspec as an executable (rather than as separate test suite), which enables hot reloading of both library and tests via ghcid (cf. `just test`).
--
-- Before production, separate out these tests to keep main executable trimmed.
import Test.Hspec (hspec)

import Vira.Lib.Git qualified

main :: IO ()
main = hspec $ do
  Vira.Lib.Git.spec
