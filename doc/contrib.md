# Contributing

Before opening PRs, please collaborate on [a project space](https://nixos.asia/en/#community).

## direnv

If any of the `.cabal` or relevant `.nix` files change, you must manually `direnv reload`, as we don't use `watch_file` in `.envrv` for a reason (see comments).

## CI

We dogfood Vira by not using GitHub Actions. Vira will "signoff" (see top-level `vira.hs`) on successful build, which will reflect in the commit status on GitHub. PRs should only be merged with green status on both platforms—aarch65-darwin and x86_64-linux.

### PR workflow

1. Open PR
1. Run Vira on two platforms — macOS & Linux.
1. Build your PR's branch on both the Vira instances
1. Confirm gh-signoff status success on GitHub UI.

> [!NOTE]
> This workflow will fully be automated once https://github.com/juspay/vira/issues/37 is in place.
