{
  description = "Example NixOS configuration with Vira";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    vira.url = "github:juspay/vira/nixos-module";
  };

  outputs = { self, nixpkgs, vira }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      nixosConfigurations.example = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          # Import the Vira module from the parent flake
          vira.nixosModules.vira

          # Example Vira configuration
          {
            services.vira = {
              enable = true;
              hostname = "0.0.0.0";
              port = 8080;
              https = false;
              stateDirectory = "/var/lib/vira";
              package = vira.packages.${system}.default;
            };

            # Minimal system configuration
            system.stateVersion = "24.05";
            boot.loader.systemd-boot.enable = true;
            boot.loader.efi.canTouchEfiVariables = true;
            fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };
            fileSystems."/boot" = { device = "/dev/disk/by-label/boot"; fsType = "vfat"; };
          }
        ];
      };
    };
}
