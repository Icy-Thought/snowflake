{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.networking;
in {
  options.modules.networking = {
    enable = mkBoolOpt false;
    networkManager = mkBoolOpt true;
    systemdNetwork = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Global useDHCP => deprecated.
      networking.useDHCP = false;
    }

    (mkIf cfg.networkManager.enable {
      systemd.services.NetworkManager-wait-online.enable = false;

      networking.networkmanager = {
        enable = mkDefault true;
        wifi.backend = "iwd";
      };
    })

    # TODO: add network connections + agenix.
    (mkIf (cfg.systemdNetwork.enable) {
      systemd.network.enable = true;

      systemd.services = {
        systemd-networkd-wait-online.enable = false;
        systemd-networkd.restartIfChanged = false;
        firewall.restartIfChanged = false;
      };

      networking.interfaces = {
        enp1s0.useDHCP = true;
        wlan0.useDHCP = true;
      };
    })
  ]);
}
