# Nix Modules

## NixOS Module

Vira provides a NixOS module for easy deployment. See the [NixOS configuration example](https://github.com/juspay/vira/blob/main/nix/examples/nixos/flake.nix) for usage.

## Home Manager Module

Vira provides a Home Manager module for running Vira as a user service. Supports Linux (systemd user services) and macOS (launchd agents).

> [!warning]
> The home-manager service may stop for any reason on macOS. You can bring it back by running:
>
> ```sh
> launchctl load ~/Library/LaunchAgents/org.vira.service.plist
> ```

See the [Home Manager configuration example](https://github.com/juspay/vira/blob/main/nix/examples/home-manager/flake.nix) for usage.
