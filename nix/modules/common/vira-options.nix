{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config;

  # Generate initial state JSON from configuration
  initialStateJson = pkgs.writeText "vira-initial-state.json" (builtins.toJSON cfg.initialState);

  hasInitialState = cfg.initialState.repositories != { };
in
{
  options = {
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

    autoResetState = mkOption {
      type = types.bool;
      default = false;
      description = "Automatically reset state on schema mismatch (removes ViraState and job workspaces)";
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
        };
      };
    };

    # Read-only computed outputs
    outputs = mkOption {
      type = types.submodule {
        options = {
          serviceCommand = mkOption {
            type = types.str;
            readOnly = true;
            description = "The computed command line to run Vira service";
            default =
              let
                globalArgs = [
                  "--state-dir"
                  cfg.stateDir
                ] ++ optionals cfg.autoResetState [ "--auto-reset-state" ];
                webArgs = [
                  "--host"
                  cfg.hostname
                  "--port"
                  (toString cfg.port)
                  "--base-path"
                  cfg.basePath
                ] ++ optionals (!cfg.https) [ "--no-https" ]
                ++ optionals hasInitialState [ "--import" initialStateJson ];
              in
              "${cfg.package}/bin/vira ${concatStringsSep " " globalArgs} web ${concatStringsSep " " webArgs}";
          };
        };
      };
      default = { };
    };
  };
}
