# CRITICAL SETUP STEPS

1. **MUST** acquire the Haskell skill before doing ANY Haskell work
2. If skill load fails, STOP and report the error - don't proceed

# Git

- DO NOT AUTOCOMMIT
- Do not run any `git` commands.

# Design System

- We use TailwindCSS as encoded in the Lucid HTML in the Haskell sources.
- Follow `DESIGN.md` guidelines.
- Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

# Local Haskell packages

Our Haskell packages are kept under ./packages directory. Whenever adding or removing items to this directory, you must also remember to update the following files: cabal.project, cabal-repl, .ghcid.

The main package, `packages/vira`, is what you can expect to be working on most of the times.

# Reporting

Sacrifice grammar for the sake of concision
