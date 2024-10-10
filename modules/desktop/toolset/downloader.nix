{ options, config, lib, pkgs, ... }:

let
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.toolset.downloader;
in {
  options.modules.desktop.toolset.downloader =
    let inherit (lib.options) mkEnableOption;
    in { transmission.enable = mkEnableOption "BitTorrent client"; };

  # TODO <- remote vs gui
  config = mkMerge [
    (mkIf cfg.transmission.enable {
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
