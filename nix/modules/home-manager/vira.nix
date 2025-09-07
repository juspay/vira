{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.vira;
  commonOptions = import ../common/vira-options.nix { inherit lib; };

  # Generate initial state JSON from configuration
  initialStateJson = pkgs.writeText "vira-initial-state.json" (builtins.toJSON cfg.initialState);

  hasInitialState = cfg.initialState.repositories != { } || cfg.initialState.cachixSettings != null || cfg.initialState.atticSettings != null;

  # Service command
  serviceCommand =
    let
      globalArgs = [
        "--state-dir"
        cfg.stateDir
      ];
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
in
{
  options.services.vira = commonOptions // {
    # Home-manager specific: override default stateDir
    stateDir = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/.local/share/vira/state";
      description = "Directory to store Vira state data";
    };
  };

  config = mkIf cfg.enable {
    # Ensure state directory exists
    home.file."${cfg.stateDir}/.keep".text = "";

    # Linux systemd user service
    systemd.user.services.vira = mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Vira web application";
        After = [ "network.target" ];
        Wants = [ "network.target" ];
      };

      Service = {
        Type = "exec";
        ExecStart = serviceCommand;
        Restart = "on-failure";
        RestartSec = "5s";

        # Security settings for user service
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = "read-only";
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.stateDir ];

        # Environment
        Environment = [
          "PATH=${makeBinPath cfg.extraPackages}:$PATH"
        ];
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    # Darwin launchd service
    launchd.agents.vira = mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        Label = "org.vira.service";
        ProgramArguments = [ "${pkgs.bash}/bin/bash" "-c" serviceCommand ];
        RunAtLoad = true;
        KeepAlive = {
          SuccessfulExit = false;
          Crashed = true;
        };
        ProcessType = "Background";
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/vira.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/vira.log";
        EnvironmentVariables = {
          PATH = "${makeBinPath cfg.extraPackages}:${config.home.sessionVariables.PATH or ""}";
        };
      };
    };

    # Add packages to user environment if requested
    home.packages = cfg.extraPackages;
  };
}
