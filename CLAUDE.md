See README.md for general project information.

## Coding Guidelines

- Never edit `.cabal` files directly; we use hpack. And never put modules explicitly in `package.yaml`.

### Haskell Coding Style

- Use `LambdaCase` and `where` for local functions
- Use `relude` over Prelude
- Use `staticWhich` for runtime dependencies (see `Vira.Lib.*`)
- Use OverloadedRecordDot syntax for field access (e.g., `record.field`) - requires `{-# LANGUAGE OverloadedRecordDot #-}` extension
- **Record field naming**: Use short, descriptive names WITHOUT type prefixes
  - ✓ Good: `data User = User { name :: Text, email :: Text }`
  - ✗ Bad: `data User = User { userName :: Text, userEmail :: Text }`
  - The type provides context; OverloadedRecordDot makes prefixes redundant
  - Enable `{-# LANGUAGE DuplicateRecordFields #-}` when field name conflicts occur
- Sync code changes with DESIGN.md

### Build instructions

**IMPORTANT**: Do not run build commands yourself. The human runs ghcid on the terminal, which then updates `ghcid.log` with any compile error or warning (if this file does not exist, or if ghcid has stopped, remind the human address it). You should read `ghcid.log` (in _entirety_) after making code changes; this file updates in a second or so. Don't rely on VSCode diagnostics.

## Git

- DO NOT AUTOCOMMIT
- Do not run any `git` commands.

## Design System

- We use TailwindCSS as encoded in the Lucid HTML in the Haskell sources.
- Follow `DESIGN.md` guidelines.
- Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

## Reporting

Sacrifice grammar for the sake of concision
