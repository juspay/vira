{ pkgs, ... }:
{
  settings = {
    attic = {
      extraBuildDepends = [
        pkgs.attic-client # For attic
        # Interactive wrapper for attic login
        (pkgs.writeShellApplication {
          name = "attic-login-interactive";
          runtimeInputs = [ pkgs.attic-client ];
          text = ''
            if [ $# -ne 2 ]; then
              echo "Usage: attic-login-interactive <server-name> <endpoint>"
              echo "Example: attic-login-interactive cache-example-com https://cache.example.com"
              exit 1
            fi

            SERVER_NAME="$1"
            ENDPOINT="$2"

            echo "Attic Login Setup"
            echo "================="
            echo "Server: $SERVER_NAME"
            echo "Endpoint: $ENDPOINT"
            echo
            read -rsp "Authentication token: " TOKEN
            echo
            echo
            echo "Logging in..."
            attic login "$SERVER_NAME" "$ENDPOINT" "$TOKEN"
            echo "Done!"
          '';
          meta.description = "Interactive attic login setup - prompts for token, takes server name and endpoint as arguments";
        })
      ];
    };
  };
}
