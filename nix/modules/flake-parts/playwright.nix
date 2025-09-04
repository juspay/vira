{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { pkgs, system, ... }:
    let
      # Use nixpkgs-25-05 for nodejs, npm, and playwright (working on darwin)
      nixpkgs-25-05 = inputs.nixpkgs-25-05.legacyPackages.${system};

      # Common Playwright environment setup
      playwrightEnv = ''
        export PLAYWRIGHT_BROWSERS_PATH=${nixpkgs-25-05.playwright-driver.browsers}
        export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
        export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
        export PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH=${nixpkgs-25-05.playwright-driver.browsers}/chromium-1169/${if nixpkgs-25-05.stdenv.isDarwin then "chrome-mac/Chromium.app/Contents/MacOS/Chromium" else "chrome-linux/chrome"}
      '';

      # Common packages for Playwright (from nixpkgs-25-05 for darwin compatibility)
      playwrightPackages = [
        nixpkgs-25-05.nodejs
        nixpkgs-25-05.nodePackages.npm
        nixpkgs-25-05.playwright-driver.browsers
      ];
    in
    {
      # Playwright testing environment
      devShells.playwright = pkgs.mkShell {
        name = "vira-playwright-shell";
        meta.description = "Playwright testing environment for Vira";
        packages = playwrightPackages;
        shellHook = ''
          ${playwrightEnv}
        '';
      };

      # Flake app for running e2e tests
      apps.e2e.program = pkgs.writeShellApplication {
        name = "run-e2e-tests";
        runtimeInputs = playwrightPackages;
        text = ''
          ${playwrightEnv}
          
          # Change to tests/e2e directory
          cd tests/e2e
          
          # Install dependencies if needed
          if [ ! -d node_modules ]; then
            echo "Installing npm dependencies..."
            npm install
          fi
          
          # Run tests
          echo "Running Playwright e2e tests..."
          npm test
        '';
      };

    };
}
