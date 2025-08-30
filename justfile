default:
    @just --list

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
