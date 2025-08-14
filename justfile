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

# Run cabal tests (hspec)
[group('2. haskell')]
test:
    ghcid --warnings -c "cabal repl vira:exe:vira-tests --flags=ghcid" --test "main"

# Delete and recreate vira.db
[group('1. vira')]
resetdb:
    rm -rf ./state
