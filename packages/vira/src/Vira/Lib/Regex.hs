{- | Module for defining simple regex utils

For more sophisticated needs, use a package listed in https://wiki.haskell.org/Regular_expressions
-}
module Vira.Lib.Regex where

{- | Naive match a pattern with '*' wildcard against an input string

Example: matchWildcard "release-*/*" "release-foo/bar"
-}
matchWildcard :: String -> String -> Bool
matchWildcard [] [] = True
matchWildcard ('*' : ps) [] = matchWildcard ps []
matchWildcard ('*' : ps) (i : is) = matchWildcard ps (i : is) || matchWildcard ('*' : ps) is
matchWildcard (p : ps) (i : is)
  | p == i = matchWildcard ps is
  | otherwise = False
matchWildcard _ _ = False
