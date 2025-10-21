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

## See Also

- [Nix Manual: Remote Builds](https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html)
- [Home Manager: nix.buildMachines](https://nix-community.github.io/home-manager/options.xhtml#opt-nix.buildMachines)
