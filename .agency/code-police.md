# code-police config

## haskell-skill

For any Haskell-facing change, use the skill tool to invoke the `haskell` skill as an additional code-police rule.

A Haskell-facing change includes `*.hs`, `*.cabal`, `cabal.project`, `package.yaml`, `.ghcid`, `ghcid.txt`, or Haskell-related build/test configuration.

If the package uses `relude`, let the `haskell` skill apply its relude guidance.

Report this rule in the Pass 1 checklist as `haskell-skill`.
