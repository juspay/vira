# Nix Modules

## NixOS Module

Vira provides a NixOS module for easy deployment. See the [NixOS configuration example](https://github.com/juspay/vira/blob/main/nix/examples/nixos/flake.nix) for usage.

## Home Manager Module

Vira provides a Home Manager module for running Vira as a user service. Supports Linux (systemd user services) and macOS (launchd agents).

### macOS Limitations

> [!warning]
> The home-manager service has several limitations on macOS due to launchd requirements:

#### Headless Mac Issue

On headless macOS systems (servers without GUI), the service will fail to start with the error:

```
Failed to start agent 'gui/502/org.vira.service' with error: Bootstrap failed: 125: Domain does not support specified action
```

**Workaround:** Log into the GUI session first, then lock the user immediately. The service requires an active GUI session context to bootstrap properly, even if running in the background.

#### Service Restart

> The home-manager service may stop for any reason on macOS. You can bring it back by running:
>
> ```sh
> launchctl load ~/Library/LaunchAgents/org.vira.service.plist
> ```

### Configuration

See the [Home Manager configuration example](https://github.com/juspay/vira/blob/main/nix/examples/home-manager/flake.nix) for usage.
