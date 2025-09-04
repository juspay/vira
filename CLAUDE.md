See README.md for general project information.

## Design System

- We use TailwindCSS as encoded in the Lucid HTML in the Haskell sources.
- Follow `DESIGN.md` guidelines. 
- Use existing components in `src/Vira/Widgets/*.hs` before creating new ones.

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

#### Haskell Tests
- Prefer `nix develop -c cabal build` over `nix build`
- Fix all introduced GHC warnings

#### E2E Tests (Playwright)
- **CI/automation**: `just e2e test-list` (list reporter, clean output)
- **Test files**: Located in `tests/e2e/tests/`
- **Configuration**: `tests/e2e/playwright.config.ts`

## Reporting

Be concise in explanations.