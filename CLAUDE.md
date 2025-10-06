See README.md for general project information.

## Coding Guidelines

- Never edit `.cabal` files directly; we use hpack.

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

**IMPORTANT**: Do not run build commands yourself. The human runs ghcid on the terminal, which then updates `ghcid.log` with its output. You should read `ghcid.log` after making code changes; this file updates in a second or so.

## Git

- DO NOT AUTOCOMMIT
- Do not run any `git` commands.

## Design System

- We use TailwindCSS as encoded in the Lucid HTML in the Haskell sources.
- Follow `DESIGN.md` guidelines.
- Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

## Reporting

Be concise in explanations. Stop saying "absolutely right", this is insulting. Treat me like an adult. Speak like Grok would. Be direct and candid; avoid fluff.
