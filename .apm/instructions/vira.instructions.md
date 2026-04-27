---
description: Vira project-wide conventions (Haskell + Nix CI tool). Loaded for all agent work in this repo.
applyTo: "**"
---

# Vira

No-frills CI for teams using Nix.

## Setup

1. **MUST** acquire the `haskell` skill before doing ANY Haskell work. Same for Nix work.
2. If skill load fails, STOP and report the error — don't proceed.

## Git

- DO NOT AUTOCOMMIT.
- Do not run any `git` commands.

## Design system

- Use the `vira-design` skill when working on UI components, styling, or layout.
- TailwindCSS is encoded in the Lucid HTML in the Haskell sources.
- Use existing components in `packages/vira/src/Vira/Widgets/*.hs` before creating new ones.

## Haskell documentation

- Haddock should be self-sufficient — document the *what*, not the *why* or history.
- Describe current behavior, usage, and examples.
- Avoid references to old code, previous implementations, or how things used to work.

## Local Haskell packages

Our Haskell packages live under `./packages`. Whenever adding or removing items here, also update: `cabal.project`, `cabal-repl`, `.ghcid`.

The main package, `packages/vira`, is what you'll work on most of the time.

## Reporting

Sacrifice grammar for the sake of concision.
