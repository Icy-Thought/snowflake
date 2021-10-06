{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  # Global useDHCP => deprecated.
  networking.useDHCP = false;

  # Per-interface useDHCP is mandatory. (Not Required by NetworkManager)
  # interfaces = {
  #   enp1s0.useDHCP = true;
  #   wlan0.useDHCP = true;
  # };

  networking.networkmanager.enable = mkDefault true;
  networking.networkmanager.wifi.backend = "iwd";
  networking.iproute2.enable = true;

  # 443 = openssl, 51820 = Wireguard, 1714 - 1764 = KDE-Connect
  networking.firewall.allowedTCPPorts = [ 53 443 ];
  networking.firewall.allowedUDPPorts = [ 53 51820 ];

  networking.firewall.allowedTCPPortRanges = [{
    from = 1714;
    to = 1764;
  }];

  networking.firewall.allowedUDPPortRanges = [{
    from = 1714;
    to = 1764;
  }];
}
