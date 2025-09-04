default:
    @just --list

mod e2e 'tests/e2e'

# Run hoogle
[group('2. haskell')]
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run the application, re-compiling if necessary.
[group('1. vira')]
run:
    vira-dev --no-server --tui=false

# Run cabal tests (Pass, for example, `tail-test` to run for different component)
[group('2. haskell')]
test COMPONENT='vira-tests':
    ghcid --warnings -T Main.main -c "./cabal-repl {{ COMPONENT }}"

[group('2. haskell')]
ghcid COMPONENT='vira':
    ghcid --warnings -c "./cabal-repl {{ COMPONENT }}"

# Delete and recreate vira.db
[group('1. vira')]
resetdb:
    rm -rf ./state

[private]
pc_hooks:
    yq -r '.repos[].hooks[].name' .pre-commit-config.yaml | \
      grep -v hpack 
[private]
git_not_added:
    git status --porcelain | awk '{print $2}'

# Run pre-commit hooks on changed and untracked files
pc:
    # Run all but hpack, since that's slower
    for hook in $(just pc_hooks); do \
        pre-commit run $hook --files $(just git_not_added) ; \
    done
    # Then run hpack manually
    @just hpack

# Re-generate .cabal files
# This is faster than running pre-commit.
hpack:
    for f in $(find ./packages/ -name "package.yaml"); do \
        hpack $f; \
    done