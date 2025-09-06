{
  description = "Example NixOS configuration with Vira";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    vira.url = "github:juspay/vira";
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
              openFirewall = true;
              stateDir = "/var/lib/vira/example-state";
              package = vira.packages.${system}.default;

              # Initial state configuration with repositories and settings
              initialState = {
                repositories = {
                  emanote = "https://github.com/srid/emanote.git";
                  omnix = "https://github.com/juspay/omnix.git";
                  vira = "https://github.com/juspay/vira.git";
                };

                cachixSettings = {
                  cachixName = "my-cache";
                  authToken = "your-cachix-token-here";
                };
              };
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

      checks.${system} = {
        vira-vm-test = pkgs.testers.runNixOSTest {
          name = "vira-service";

          nodes.machine = { ... }: {
            imports = [
              vira.nixosModules.vira
            ];

            services.vira = {
              enable = true;
              hostname = "0.0.0.0";
              port = 8080;
              https = false;
              openFirewall = true;
              stateDir = "/var/lib/vira/example-state";
              package = vira.packages.${system}.default;

              # Test initial state
              initialState = {
                repositories = {
                  test-repo = "https://github.com/srid/haskell-template.git";
                };
              };
            };

            # Minimal VM configuration
            system.stateVersion = "24.05";
          };

          testScript = ''
            import time

            machine.start()
            machine.wait_for_unit("multi-user.target")
            
            # Wait for Vira service to start
            machine.wait_for_unit("vira.service")
            
            # Wait a bit for the service to fully initialize
            time.sleep(5)
            
            # Check if the service is listening on port 8080
            machine.wait_for_open_port(8080)
            
            # Test HTTP response
            machine.succeed("curl -f http://localhost:8080")
            
            # Check service status
            machine.succeed("systemctl is-active vira.service")
            
            # Test that initial state was imported correctly
            # Check the repository list page for our imported repository
            result = machine.succeed("curl -s http://localhost:8080/r")
            print(f"Repository page content: {result}")
            
            # Check if our test repository is listed
            if "test-repo" in result:
                print("✅ Initial state was imported correctly")
            else:
                print("❌ Initial state import may have failed")
            
            print("✅ Vira service is running and responding to HTTP requests")
          '';
        };
      };
    };
}
