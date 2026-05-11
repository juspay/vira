---
name: haskell
description: Use this when writing or reviewing Haskell code. Covers error handling, type safety, idiomatic patterns, HLint compliance, Aeson usage, and testing.
---

# Haskell Development

## Error Handling

- NEVER use `undefined` or `error` as placeholders
- Handle ALL `Maybe`/`Either` cases explicitly — no silent ignoring
- Handle all pattern match cases — no partial matches
- Use types to make impossible states unrepresentable

## Type Safety & Idioms

- Write type signatures for all top-level definitions
- Prefer `Text` over `String`
- Use `newtype` wrappers for domain types
- Apply smart constructors for validation
- Write total functions — avoid `head`, `tail`, and other partial functions
- Prefer pure functions over IO
- Use explicit module exports
- Favor composition over complex functions
- Write Haddock documentation for public APIs

## Records

- Use `OverloadedRecordDot` (add pragma to modules that use the syntax)
- Use `DisambiguateRecordFields` and `DuplicateRecordFields` for simple field names
- Use lenses for record manipulation when appropriate

## Aeson

- NEVER construct aeson objects by hand
- Create a type and use `encode`/`decode` on it
- Prefer generic deriving; hand-write instances only when the wire format requires it

## HLint

If `.hlint.yaml` exists in the project root, run `hlint` on every modified file. Fix ALL warnings before considering the task complete.

## Build Workflow

If `ghcid.txt` exists in the project, check it after every code change for compile errors. Do not proceed until it is clean. If this file does not exist, use whatever build workflow the project documents.

When adding or deleting `.hs` modules: update the `.cabal` file, or run `hpack` if `package.yaml` exists.

## Relude

If the project uses [relude](https://github.com/kowainik/relude) (check `.cabal` or `package.yaml` dependencies), also follow [RELUDE.md](RELUDE.md) for idiomatic substitutions.

## Testing

- QuickCheck for property-based testing
- HUnit or Hspec for unit tests
