See README.md for general project information.

## Design System

Follow `DESIGN.md` guidelines. Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

## Coding Guidelines

### Haskell Style
- Use `LambdaCase` and `where` for local functions
- Fix all GHC warnings
- Use `relude` over Prelude
- Use `staticWhich` for runtime dependencies (see `Vira.Lib.Git`)

### Project Structure
- `Vira.Lib.*` - Future 3rd party libraries (no external Vira types)
- Follow Volatility-Based Decomposition
- Sync code changes with DESIGN.md
- Regenerate cabal: `pre-commit run -a`

## Development

### Testing
- Prefer `cabal build` over running app
- Fix all introduced GHC warnings

## Reporting

Be concise in explanations.