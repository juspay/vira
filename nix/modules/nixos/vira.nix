{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.vira;
  commonOptions = import ../common/vira-options.nix { inherit lib; };

  # Generate initial state JSON from configuration
  initialStateJson = pkgs.writeText "vira-initial-state.json" (builtins.toJSON cfg.initialState);

  hasInitialState = cfg.initialState.repositories != { } || cfg.initialState.cachixSettings != null || cfg.initialState.atticSettings != null;
in
{
  options.services.vira = commonOptions // {
    # NixOS-specific options
    user = mkOption {
      type = types.str;
      default = "vira";
      description = "User to run Vira as";
    };

    group = mkOption {
      type = types.str;
      default = "vira";
      description = "Group to run Vira as";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to open the firewall for the Vira port";
    };

    # Override default stateDir for NixOS
    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/vira/state";
      description = "Directory to store Vira state data";
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = "/var/lib/vira";
      createHome = true;
    };

    systemd.tmpfiles.rules = [
      "d '${cfg.stateDir}' 0755 ${cfg.user} ${cfg.group} - -"
    ];

    users.groups.${cfg.group} = { };

    systemd.services.vira = {
      description = "Vira web application";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path = cfg.extraPackages;

      serviceConfig = {
        Type = "exec";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = "/var/lib/vira";

        # Security settings
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ "/var/lib/vira" cfg.stateDir ];

        # Restart settings
        Restart = "on-failure";
        RestartSec = "5s";

        ExecStart =
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
      };
    };

    # Open firewall port if requested
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];
  };
}
