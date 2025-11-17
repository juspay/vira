# Contributing

If you are working a non-trivial feature, you can [discuss it first](https://github.com/juspay/vira/discussions).

## direnv

If any of the `.cabal` or relevant `.nix` files change, you must manually `direnv reload`, as we don't use `watch_file` in `.envrv` for a reason (see comments).

## VSCode

### Extensions

Install recommended extensions (VSCode will prompt on first open). Includes direnv extension for running VSCode in Nix devshell env. See [nixos.asia/vscode#direnv](https://nixos.asia/en/vscode#direnv) for details.

### Tasks

Launch all dev tools (Claude, GHCid, Hpack Watch) at once:

**Command Palette**: `Cmd/Ctrl+Shift+P` → "Tasks: Run Task" → "Start All Dev Tools"

Or add keyboard shortcut to `.vscode/keybindings.json`:

```json
[
  {
    "key": "cmd+shift+t",
    "command": "workbench.action.tasks.runTask",
    "args": "Start All Dev Tools"
  }
]
```

## CI

We dogfood Vira by not using GitHub Actions. Vira (Srid's instance) will "signoff" (see top-level `vira.hs`) on successful build for both platforms, which will reflect in the commit status on GitHub. PRs should only be merged with green status.

You can also run CI locally using:

```sh
just ci  # In nix devshell
```

But this will build only for the current platform.
