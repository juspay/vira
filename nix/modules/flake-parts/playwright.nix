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
        processes = let host = "127.0.0.1"; port = "5006"; in {
          vira-server = {
            command = pkgs.writeShellApplication {
              name = "vira-test-server";
              runtimeInputs = [ config.packages.default ];
              text = ''
                # Clean start for tests
                rm -rf ./state
                mkdir -p ./state
                rm -f /tmp/playwright-tests-complete
                rm -f /tmp/playwright-test-result
                
                # Start Vira server for testing in background
                vira --host ${host} --port ${port} --state-dir ./state --base-path / &
                VIRA_PID=$!
                
                # Monitor for test completion
                while [ ! -f /tmp/playwright-tests-complete ]; do
                  sleep 1
                  # Check if vira is still running
                  if ! kill -0 $VIRA_PID 2>/dev/null; then
                    echo "Vira server stopped unexpectedly"
                    exit 1
                  fi
                done
                
                echo "Tests completed, shutting down Vira server"
                kill $VIRA_PID 2>/dev/null || true
                wait $VIRA_PID 2>/dev/null || true
                exit 0
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
            availability.restart = "no";
          };

          playwright-tests = {
            command = pkgs.writeShellApplication {
              name = "run-playwright-tests";
              runtimeInputs = [ pkgs.nodejs pkgs.nodePackages.npm ];
              text = ''
                # Change to the tests directory (assuming we're in project root)
                if [ -d tests ]; then
                  cd tests
                elif [ -d ../tests ]; then
                  cd ../tests
                else
                  echo "Error: Could not find tests directory"
                  exit 1
                fi
                
                # Install dependencies if needed
                if [ ! -d node_modules ]; then
                  echo "Installing npm dependencies..."
                  npm install
                fi
                
                # Set Playwright environment variables
                export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
                export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
                export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
                
                # Run tests
                echo "Running Playwright tests..."
                TEST_RESULT=0
                npm test || TEST_RESULT=$?
                
                # Signal that tests are complete and save result
                echo "Tests completed" > /tmp/playwright-tests-complete
                echo "$TEST_RESULT" > /tmp/playwright-test-result
                
                if [ $TEST_RESULT -eq 0 ]; then
                  echo "✅ All Playwright tests passed!"
                  exit 0
                else
                  echo "❌ Some Playwright tests failed!"
                  exit 1
                fi
              '';
            };
            depends_on.vira-server.condition = "process_healthy";
            availability.restart = "no";
          };
        };
      };
    };

    # Convenience package for running the full test suite with proper exit behavior
    packages.playwright-tests = pkgs.writeShellScriptBin "vira-playwright-tests" ''
      set -uo pipefail
      echo "Starting Vira server and running Playwright tests..."
      
      # Run the process-compose script with TUI disabled and capture exit code
      set +e
      ${config.process-compose.playwright-test.outputs.package}/bin/playwright-test --tui=false "$@"
      PC_EXIT_CODE=$?
      set -e
      
      # Check if tests passed or failed based on marker file and exit with proper code
      if [ -f /tmp/playwright-tests-complete ]; then
        if [ -f /tmp/playwright-test-result ]; then
          TEST_RESULT=$(cat /tmp/playwright-test-result)
          echo "Test result: $TEST_RESULT"
          exit $TEST_RESULT
        fi
      fi
      
      # Fallback to process-compose exit code
      exit $PC_EXIT_CODE
    '';
  };
}
