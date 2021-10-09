{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.networking;
in {
  options.modules.networking = {
    enable = mkBoolOpt false;
    networkManager = mkBoolOpt false;
    systemdNetwork = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Global useDHCP => deprecated.
      networking.useDHCP = false;
    }

    (mkIf networkManager.enable {
      networking.networkmanager.enable = mkDefault true;
      networking.networkmanager.wifi.backend = "iwd";
      systemd.services.NetworkManager-wait-online.enable = false;
    })

    (mkIf systemdNetwork.enable {
      # TODO: add network connections + agenix.
      systemd.network.enable = true;

      # often hangs
      systemd.services.systemd-networkd-wait-online.enable = false;
      # sometimes cannot be restarted -> breaks system upgrade
      systemd.services.systemd-networkd.restartIfChanged = false;
      # fails to delete some chain on upgrade...
      systemd.services.firewall.restartIfChanged = false;

      interfaces.enp1s0.useDHCP = true;
      interfaces.wlan0.useDHCP = true;
    })
  ]);
}
