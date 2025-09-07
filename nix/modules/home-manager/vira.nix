{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.vira;
in
{
  options.services.vira = mkOption {
    type = types.submoduleWith {
      specialArgs = { inherit pkgs; };
      modules = [
        ../common/vira-options.nix
        {
          # Home-manager specific: override default stateDir
          config.stateDir = mkDefault "${config.home.homeDirectory}/.local/share/vira/state";
        }
      ];
    };
    default = { };
  };

  config = mkIf cfg.enable {
    # Linux systemd user service
    systemd.user.services.vira = mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Vira web application";
        After = [ "network.target" ];
        Wants = [ "network.target" ];
      };

      Service = {
        Type = "exec";
        ExecStart = cfg.outputs.serviceCommand;
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
        ProgramArguments = [
          "${pkgs.bash}/bin/bash"
          "-c"
          "${pkgs.coreutils}/bin/mkdir -p $(${pkgs.coreutils}/bin/dirname ${cfg.stateDir}) && ${cfg.outputs.serviceCommand}"
        ];
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
