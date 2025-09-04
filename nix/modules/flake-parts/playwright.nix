{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { pkgs, lib, ... }:
    let
      # Use nodejs from nixpkgs-nodejs input for darwin compatibility
      nixpkgs-nodejs = inputs.nixpkgs-nodejs.legacyPackages.${pkgs.system};

      # Common Playwright environment setup
      playwrightEnv = ''
        export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
        export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
        export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
        export PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH=${pkgs.playwright-driver.browsers}/chromium-1181/chrome-linux/chrome
      '';

      # Common packages for Playwright
      playwrightPackages = with pkgs; [
        nixpkgs-nodejs.nodejs
        nixpkgs-nodejs.nodePackages.npm
        playwright-driver.browsers
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
