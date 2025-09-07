{ lib }:

with lib;

{
  enable = mkEnableOption "Vira web application";

  package = mkOption {
    type = types.package;
    description = "The Vira package to use";
  };

  hostname = mkOption {
    type = types.str;
    default = "localhost";
    description = "Hostname to bind Vira to";
  };

  port = mkOption {
    type = types.port;
    default = 5005;
    description = "Port to bind Vira to";
  };

  https = mkOption {
    type = types.bool;
    default = true;
    description = "Enable HTTPS";
  };

  extraPackages = mkOption {
    type = types.listOf types.package;
    default = [ ];
    description = "Extra packages to add to the Vira service PATH";
  };

  stateDir = mkOption {
    type = types.str;
    description = "Directory to store Vira state data";
  };

  basePath = mkOption {
    type = types.str;
    default = "/";
    description = "Base URL path for the HTTP server";
  };

  initialState = mkOption {
    description = "Initial state configuration for Vira";
    default = { };
    type = types.submodule {
      options = {
        repositories = mkOption {
          description = "Map of repository names to clone URLs";
          default = { };
          type = types.attrsOf types.str;
        };

        cachixSettings = mkOption {
          description = "Cachix configuration";
          default = null;
          type = types.nullOr (types.submodule {
            options = {
              cachixName = mkOption {
                type = types.str;
                description = "Cachix cache name";
              };
              authToken = mkOption {
                type = types.str;
                description = "Cachix authentication token";
              };
            };
          });
        };

        atticSettings = mkOption {
          description = "Attic configuration";
          default = null;
          type = types.nullOr (types.submodule {
            options = {
              # Add attic-specific options here when needed
            };
          });
        };
      };
    };
  };
}
