{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.toolset.downloader = {
    transmission.enable = mkEnableOption "BitTorrent client";
  };

  # TODO <- remote vs gui
  config = mkMerge [
    (mkIf config.modules.desktop.toolset.downloader.transmission.enable {
      user = {
        packages = [ pkgs.transmission-gtk ];
        extraGroups = [ "transmission" ];
      };

      networking.firewall = {
        allowedTCPPorts = [ 51413 ];
        allowedUDPPorts = [ 51413 ];
      };
    })
  ];
}
