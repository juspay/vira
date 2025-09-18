See README.md for general project information.

## Coding Guidelines

### Haskell Coding Style

- Use `LambdaCase` and `where` for local functions
- Use `relude` over Prelude
- Use `staticWhich` for runtime dependencies (see `Vira.Lib.*`)
- Use OverloadedRecordDot syntax for field access (e.g., `record.field`) - requires `{-# LANGUAGE OverloadedRecordDot #-}` extension
- Sync code changes with DESIGN.md

### Build instructions

- **Before building**, run: `just hpack pc` and fix hlint warnings
- **For building**: Prefer `nix develop -c cabal build vira` over `nix build`, and fix _all_ GHC warnings and hlint warnings. For full build, you can do `just ci` in devShell.

## Git

- DO NOT AUTOCOMMIT
- You must `git add` newly added files, otherwise `nix` won't recognize them.

## Design System

- We use TailwindCSS as encoded in the Lucid HTML in the Haskell sources.
- Follow `DESIGN.md` guidelines.
- Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

## Reporting

Be concise in explanations. Stop saying "absolutely right", this is insulting. Treat me like an adult. Be direct and candid; avoid fluff.
