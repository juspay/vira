{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { config, pkgs, lib, ... }: {
    # Playwright testing environment
    devShells.playwright = pkgs.mkShell {
      name = "vira-playwright-shell";
      meta.description = "Playwright testing environment for Vira";
      inputsFrom = [
        config.devShells.default
      ];
      packages = with pkgs; [
        nodejs
        nodePackages.npm
        playwright-driver.browsers
      ];
      shellHook = ''
        export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
        export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
        echo "Playwright testing environment loaded"
        echo "Run 'nix run .#playwright-test' to run integration tests"
      '';
    };

    # Process compose configuration for running Vira + Playwright tests
    process-compose."playwright-test" = {
      settings = {
        processes = let host = "127.0.0.1"; port = "5005"; in {
          vira-server = {
            command = pkgs.writeShellApplication {
              name = "vira-test-server";
              runtimeInputs = [ config.packages.default ];
              text = ''
                # Clean start for tests
                rm -rf ./state
                mkdir -p ./state
                
                # Start Vira server for testing
                vira --host ${host} --port ${port} --state-dir ./state --base-path /
              '';
            };
            readiness_probe = {
              exec.command = lib.getExe (pkgs.writeShellApplication {
                name = "vira-test-health";
                runtimeInputs = [ pkgs.curl ];
                text = ''
                  curl -k -f https://${host}:${port} > /dev/null 2>&1
                '';
                meta.description = "Health check for Vira test server";
              });
              initial_delay_seconds = 5;
              period_seconds = 2;
              timeout_seconds = 5;
              success_threshold = 1;
              failure_threshold = 10;
            };
            shutdown.signal = 2; # SIGINT for graceful shutdown
          };

          playwright-tests = {
            command = pkgs.writeShellApplication {
              name = "run-playwright-tests";
              runtimeInputs = [ pkgs.nodejs pkgs.nodePackages.npm ];
              text = ''
                cd tests
                
                # Install dependencies if needed
                if [ ! -d node_modules ]; then
                  echo "Installing npm dependencies..."
                  npm install
                fi
                
                # Set Playwright environment variables
                export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
                export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
                
                # Run tests
                echo "Running Playwright tests..."
                npm test
              '';
            };
            depends_on.vira-server.condition = "process_healthy";
          };
        };
      };
    };

    # Convenience package for running the full test suite
    packages.playwright-tests = config.process-compose.playwright-test.outputs.package;
  };
}
