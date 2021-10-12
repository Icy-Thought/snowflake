{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.transmission;
in {
  options.modules.services.transmission = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ transmission-gtk ];

    services.transmission = {
      home = "${config.user.home}/Torrents";
      settings = {
        # Manage transmisson file locations:
        rename-partial-files = true;
        incomplete-dir-enabled = true;
        watch-dir-enabled = false;

        # Control transmisson ports:
        peer-port = 51413;
        peer-port-random-high = 65535;
        peer-port-random-low = 49152;
        peer-port-random-on-start = false;
        rpc-port = 9091;
        rpc-whitelist-enabled = true;
        rpc-whitelist = "127.0.0.1,192.168.*.*";
        rpc-bind-address = "0.0.0.0";

        # Extras settings:
        message-level = 1;
        umask = 2;
        utp-enabled = true;
        ratio-limit = 0;
        encryption = 2;
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
