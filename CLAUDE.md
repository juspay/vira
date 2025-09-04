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
- **Page objects**: Located in `tests/e2e/pages/` following Page Object Model pattern
- **Configuration**: `tests/e2e/playwright.config.ts`

##### Best Practices

- **Page Object Model**: Use page objects to encapsulate page interactions and selectors
- **Test isolation**: Use `test.beforeEach` and `test.afterEach` for setup/cleanup. Each test should be independent
- **Hierarchical structure**: Use `test.describe` blocks to organize related tests logically
- **Selector guidelines**: Use semantic selectors in order: `getByLabel()` > `getByRole()` > `getByTestId()` > CSS selectors. Avoid placeholders
- **Style guidelines**: Write concise, functional-style tests. Avoid intermediate variables. Group related actions. Minimal comments
- **Timing**: Use `waitForURL()` and `waitForSelector()` with appropriate timeouts for async operations
- **Code analysis**: Always examine Haskell source code to understand exact DOM structure, routes, and CSS classes rather than guessing
- **Cleanup**: Implement proper cleanup in `afterEach` hooks to prevent test interference

## Reporting

Be concise in explanations.
