{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.transmission;
in {
  options.modules.services.transmission = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ transmission-remote-gtk ];

    services.transmission = {
      enable = true;
      settings = {
        # Manage transmisson file locations:
        download-dir = "/var/lib/transmission/Downloads";
        incomplete-dir = "/var/lib/transmission/.incomplete";
        rename-partial-files = true;
        incomplete-dir-enabled = true;
        watch-dir-enabled = false;

        # Control transmisson ports:
        rpc-port = 9091;
        rpc-whitelist-enabled = true;
        rpc-whitelist = "127.0.0.1,192.168.*.*";
        rpc-bind-address = "0.0.0.0";

        peer-port = 51413;
        peer-socket-tos = "lowcost";

        # Extras settings:
        umask = 2;
        encryption = 2;
        message-level = 1;
        utp-enabled = true;

        ratio-limit = 0;
        ratio-limit-enabled = true;

        speed-limit-up-enabled = true;
        speed-limit-up = 1;
        idle-seeding-limit-enabled = true;
        idle-seeding-limit = 0;
      };
    };

    networking.firewall = {
      allowedTCPPorts = [ 51413 ];
      allowedUDPPorts = [ 51413 ];
    };

    user.extraGroups = [ "transmission" ];
  };
}
