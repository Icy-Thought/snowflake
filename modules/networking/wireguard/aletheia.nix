{ config, lib, pkgs, ... }:

let interface = "aletheia";
in with lib; {
  networking.wg-quick.interfaces.${interface} = {
    configFile = "/run/agenix/${interface}";
  };

  systemd.services."wg-quick-${interface}" = {
    requires = [ "network-online.target" ];
    after = [ "network.target" "network-online.target" ];
    wantedBy = mkForce [ ];
    environment.DEVICE = "${interface}";
  };
}
