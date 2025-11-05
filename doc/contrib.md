# Contributing

Before opening PRs, please collaborate on [a project space](https://github.com/juspay/vira/discussions).

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

We dogfood Vira by not using GitHub Actions. Vira will "signoff" (see top-level `vira.hs`) on successful build, which will reflect in the commit status on GitHub. PRs should only be merged with green status on both platforms—aarch65-darwin and x86_64-linux.

You can also run CI locally using:

```sh
just ci  # In nix devshell
```

### PR workflow

1. Open PR
1. Run Vira on two platforms — macOS & Linux.
1. Build your PR's branch on both the Vira instances (or run `just ci` on clean branch)
1. Confirm gh-signoff status success on GitHub UI.

> [!NOTE]
> This workflow will fully be automated once https://github.com/juspay/vira/issues/37 is in place.
