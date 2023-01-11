{ config
, options
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.toolset.downloader;
in
{
  options.modules.desktop.toolset.downloader = {
    transmission.enable = mkBoolOpt false;
  };

  # TODO <- remote vs gui
  config = mkMerge [
    (mkIf cfg.transmission.enable {
      user = {
        packages = with pkgs; [ transmission-gtk ];
        extraGroups = [ "transmission" ];
      };

      networking.firewall = {
        allowedTCPPorts = [ 51413 ];
        allowedUDPPorts = [ 51413 ];
      };
    })
  ];
}
