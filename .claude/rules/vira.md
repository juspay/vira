---
paths:
  - "**"
---

# Vira

No-frills CI for teams using Nix.

## Design system

- TailwindCSS is encoded in the Lucid HTML in the Haskell sources.
- Use existing components in `packages/vira/src/Vira/Widgets/*.hs` before creating new ones.

## Haskell documentation

- Haddock should be self-sufficient — document the *what*, not the *why* or history.
- Describe current behavior, usage, and examples.
- Avoid references to old code, previous implementations, or how things used to work.

## Local Haskell packages

Our Haskell packages live under `./packages`. Whenever adding or removing items here, also update: `cabal.project`, `cabal-repl`, `.ghcid`.

The main package, `packages/vira`, is what you'll work on most of the time.
