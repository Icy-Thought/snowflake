{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.networking.wireGuard;
in {
  options.modules.networking.wireGuard = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ wireguard ];

    systemd.services = {
      systemd-resolved.enable = true;
      systemd-machined.enable = false;
    };

    networking = {
      iproute2.enable = true;
      firewall.allowedTCPPorts = [ 53 ];
      firewall.allowedUDPPorts = [ 53 51820 ];
    };
  };
}