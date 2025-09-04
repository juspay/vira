{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { pkgs, ... }: {
    # Playwright testing environment
    devShells.playwright = pkgs.mkShell {
      name = "vira-playwright-shell";
      meta.description = "Playwright testing environment for Vira";
      packages = with pkgs; [
        nodejs
        nodePackages.npm
        playwright-driver.browsers
      ];
      shellHook = ''
        export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
        export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
        export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
        # Set browser executable for Chromium only
        export PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH=${pkgs.playwright-driver.browsers}/chromium-1181/chrome-linux/chrome
        echo "Playwright testing environment loaded"
        echo "Chromium: $PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH"
      '';
    };
  };
}
