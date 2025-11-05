---
slug: modules
---

# Nix Module

## Home Manager Module

Vira provides a Home Manager module for running Vira as a user service. Supports Linux (systemd user services) and macOS (launchd agents).

> [!note]
> Vira is only supported as a user-space service via Home Manager. System-wide deployment via NixOS modules is no longer supported, as Vira relies on user-space tools (attic, git, ssh, etc.) and user-space configuration (attic authentication tokens, SSH keys, git credentials, etc.).

### Linux: Enable Lingering

> [!warning]
> On Linux, you must enable "lingering" for your user to allow the Vira service to run as a daemon. Lingering enables systemd user services to start at boot and continue running without requiring an active login session.

**For NixOS users:**

Add the following to your NixOS configuration:

```nix
users.users.<username>.linger = true;
```

Replace `<username>` with your actual username.

**For other Linux distributions:**

Run the following command:

```sh
loginctl enable-linger $USER
```

You can verify lingering is enabled by running:

```sh
loginctl show-user $USER | grep Linger
```

This should output `Linger=yes`.

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

### Available Options

The Home Manager module supports the following configuration options:

- `enable` - Enable the Vira service (default: `false`)
- `package` - The Vira package to use
- `hostname` - Hostname to bind Vira to (default: `"localhost"`)
- `port` - Port to bind Vira to (default: `5005`)
- `https` - Enable HTTPS (default: `true`)
- `stateDir` - Directory to store Vira state data
- `basePath` - Base URL path for the HTTP server (default: `"/"`)
- `autoResetState` - Automatically reset state on schema mismatch, removing ViraState and job workspaces (default: `true`)
- `maxConcurrentBuilds` - Maximum concurrent CI builds (default: `null`, defaults to 2)
- `autoBuildNewBranches` - Auto-build new branches (default: `false`, only auto-builds branches built at least once)
- `extraPackages` - Extra packages to add to the Vira service PATH
- `initialState.repositories` - Map of repository names to clone URLs for initial state
- `systemd.environment` - Additional environment variables for the service (Linux only)
- `systemd.serviceConfig` - Additional systemd Service section attributes (Linux only)

> [!note]
> The `autoResetState` option is enabled by default to ensure smooth upgrades when Vira's internal state schema changes. If you prefer to preserve state across schema changes (and handle migrations manually), set this to `false`.

#### Systemd Service Customization

On Linux, you can customize the systemd service configuration using the `systemd` options:

**Environment Variables:**

```nix
services.vira = {
  systemd.environment = {
    GIT_SSH_COMMAND = "ssh -i /path/to/key";
    CUSTOM_VAR = "value";
  };
};
```

**Service Configuration:**

```nix
services.vira = {
  systemd.serviceConfig = {
    CPUQuota = "50%";
    MemoryMax = "2G";
  };
};
```
