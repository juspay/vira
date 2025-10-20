{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.vira;

  # SSH wrapper that bypasses systemd config to avoid permission errors
  # SSH reads system configs from /etc/ssh/ssh_config.d/ and systemd dirs by default
  # We copy the user config content (not symlink) to avoid nix store permission issues
  sshWrapper = pkgs.writeShellScript "vira-ssh-wrapper" ''
    # Create SSH config in runtime dir
    VIRA_SSH_CONFIG="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/vira-ssh-config"
    USER_SSH_CONFIG="${config.home.homeDirectory}/.ssh/config"

    # Copy user config content if it exists
    if [ -f "$USER_SSH_CONFIG" ]; then
      cat "$USER_SSH_CONFIG" > "$VIRA_SSH_CONFIG"
    else
      : > "$VIRA_SSH_CONFIG"  # Empty config
    fi

    chmod 600 "$VIRA_SSH_CONFIG"
    exec ${pkgs.openssh}/bin/ssh -F "$VIRA_SSH_CONFIG" "$@"
  '';
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

        # Environment
        Environment = [
          "PATH=${makeBinPath cfg.extraPackages}:$PATH"
          "GIT_SSH_COMMAND=${sshWrapper}"
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
        EnvironmentVariables.PATH = makeBinPath cfg.extraPackages;
      };
    };

    # Add packages to user environment if requested
    home.packages = cfg.extraPackages;
  };
}
