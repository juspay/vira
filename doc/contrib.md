# Contributing

Before opening PRs, please collaborate on [a project space](https://nixos.asia/en/#community).

## CI

We dogfood Vira by not using GitHub Actions. Vira will "signoff" (see top-level `vira.hs`) on successful build, which will reflect in the commit status on GitHub. PRs should only be merged with green status on both platforms—aarch65-darwin and x86_64-linux.

This workflow will fully be automated once https://github.com/juspay/vira/issues/37 is in place.
