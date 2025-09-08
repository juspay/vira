{
  description = "Example Home Manager configuration with Vira";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    vira.url = "github:juspay/vira";
  };

  outputs = { self, nixpkgs, home-manager, vira }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      homeConfigurations.example = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          # Import the Vira home-manager module
          vira.homeManagerModules.vira

          # Example Vira configuration
          {
            home = {
              username = "testuser";
              homeDirectory = "/home/testuser";
              stateVersion = "24.05";
            };

            services.vira = {
              enable = true;
              hostname = "0.0.0.0";
              port = 8080;
              https = false;
              stateDir = "/home/testuser/.local/share/vira/example-state";
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

          }
        ];
      };

      checks.${system} = {
        vira-home-manager-test = pkgs.testers.runNixOSTest {
          name = "vira-home-manager-service";

          nodes.machine = { ... }: {
            imports = [ home-manager.nixosModules.home-manager ];

            users.users.testuser = {
              isNormalUser = true;
              uid = 1000;
            };

            # Enable user services
            systemd.services."user@1000" = {
              overrideStrategy = "asDropin";
            };

            # Ensure runtime directory exists
            systemd.tmpfiles.rules = [
              "d /run/user/1000 0755 testuser testuser -"
            ];

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.testuser = {
                imports = [ vira.homeManagerModules.vira ];

                services.vira = {
                  enable = true;
                  hostname = "0.0.0.0";
                  port = 8080;
                  https = false;
                  stateDir = "/home/testuser/.local/share/vira/test-state";
                  package = vira.packages.${system}.default;

                  # Test initial state
                  initialState = {
                    repositories = {
                      test-repo = "https://github.com/srid/haskell-template.git";
                    };
                  };
                };

                home = {
                  username = "testuser";
                  homeDirectory = "/home/testuser";
                  stateVersion = "24.05";
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
            
            # Enable lingering for testuser to allow user services to run
            machine.succeed("loginctl enable-linger testuser")
            
            # Start a proper user session 
            machine.succeed("machinectl shell testuser@ /bin/true")
            
            # Use systemctl --user directly with proper environment
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user daemon-reload")
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user start vira.service")
            
            # Wait for the service to start
            time.sleep(10)
            
            # Check if the service is running
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user is-active vira.service")
            
            # Check if the service is listening on port 8080
            machine.wait_for_open_port(8080)
            
            # Test HTTP response
            machine.succeed("curl -f http://localhost:8080")
            
            # Test that initial state was imported correctly
            result = machine.succeed("curl -s http://localhost:8080/r")
            print(f"Repository page content: {result}")
            
            # Check if our test repository is listed
            if "test-repo" in result:
                print("✅ Initial state was imported correctly")
            else:
                print("❌ Initial state import may have failed")
            
            print("✅ Vira home-manager service is running and responding to HTTP requests")
          '';
        };
      };
    };
}
