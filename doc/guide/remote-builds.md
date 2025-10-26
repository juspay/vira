---
slug: remote-builds
---

# Remote Builds

When building for multiple systems (e.g., `build.systems = ["x86_64-linux", "aarch64-darwin"]` in [[config]]), you'll need to configure remote builders to build for systems other than your current platform.

## Home Manager Configuration

> [!WARNING]
> AI generated. Requires verification

Configure remote builders in your Home Manager configuration:

```nix
{
  nix.buildMachines = [
    {
      hostName = "builder.example.com";
      system = "aarch64-darwin";
      maxJobs = 4;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }
  ];

  nix.distributedBuilds = true;
}
```

## SSH Configuration

> [!WARNING]
> AI generated. Requires verification

Ensure SSH access to remote builders is properly configured:

```nix
{
  programs.ssh = {
    enable = true;
    matchBlocks."builder.example.com" = {
      user = "builder";
      identityFile = "~/.ssh/id_builder";
    };
  };
}
```

## Verification

TODO

## Troubleshooting

### macOS: Nix daemon disconnected unexpectedly

On a fresh macOS install with official Nix, using it as a remote builder may produce an error like:

```
cannot build on 'ssh-ng://user@hostname': error: cannot open connection to remote store 'ssh-ng://': error: Nix daemon disconnected unexpectedly (maybe it crashed?)
```

This occurs because SSH doesn't `source` the shell init files, preventing Nix from being found in the PATH. See [NixOS/nix#7508](https://github.com/NixOS/nix/issues/7508) for details.

**Workaround**: Change the macOS user's login shell to bash installed via Nix:

```sh
# Install bash via Nix
sudo nix profile install nixpkgs#bashInteractive

# Add the Nix bash to /etc/shells
echo '/nix/var/nix/profiles/default/bin/bash' | sudo tee -a /etc/shells

# Change default shell
chsh -s /nix/var/nix/profiles/default/bin/bash
```

SSH connections will now properly source Nix into the PATH.

## See Also

- [Nix Manual: Remote Builds](https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html)
- [Home Manager: nix.buildMachines](https://nix-community.github.io/home-manager/options.xhtml#opt-nix.buildMachines)
