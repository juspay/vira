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

      Service =
        let
          # Build environment variable list, avoiding duplicate PATH entries
          defaultEnv = optionalAttrs (cfg.extraPackages != [ ]) {
            PATH = "${makeBinPath cfg.extraPackages}:$PATH";
          };
          # User environment overrides defaults
          mergedEnv = defaultEnv // cfg.systemd.environment;
          envList = mapAttrsToList (name: value: "${name}=${value}") mergedEnv;
        in
        {
          Type = "exec";
          ExecStart = cfg.outputs.serviceCommand;
          Restart = "on-failure";
          RestartSec = "5s";

          # Security settings for user service
          NoNewPrivileges = true;
          PrivateTmp = true;

          # Environment
          Environment = envList;
        } // cfg.systemd.serviceConfig;

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
        EnvironmentVariables.PATH = makeBinPath cfg.extraPackages;
      };
    };

    # Add packages to user environment if requested
    home.packages = cfg.extraPackages;
  };
}
