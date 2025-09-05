{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.vira;
in
{
  options.services.vira = {
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

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = "Extra packages to add to the Vira service PATH";
    };

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
            args = [
              "--host"
              cfg.hostname
              "--port"
              (toString cfg.port)
              "--state-dir"
              cfg.stateDir
            ] ++ optionals (!cfg.https) [ "--no-https" ];
          in
          "${cfg.package}/bin/vira ${concatStringsSep " " args}";
      };
    };

    # Open firewall port if requested
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];
  };
}
