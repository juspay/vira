{ pkgs, ... }:
{
  settings = {
    attic = {
      extraBuildDepends = [
        # Interactive wrapper for attic login
        (pkgs.writeShellApplication {
          name = "attic-login-interactive";
          runtimeInputs = [ pkgs.attic-client ];
          text = ''
            echo "Attic Login Setup"
            echo "================="
            echo
            read -rp "Server name (e.g., cache-example-com): " SERVER_NAME
            read -rp "Server endpoint (e.g., https://cache.example.com): " ENDPOINT
            read -rsp "Authentication token: " TOKEN
            echo
            echo
            echo "Logging in..."
            attic login "$SERVER_NAME" "$ENDPOINT" "$TOKEN"
            echo "Done! You can now use: attic push \$SERVER_NAME:cache-name /nix/store/..."
          '';
          meta.description = "Interactive attic login setup";
        })
      ];
    };
  };
}
