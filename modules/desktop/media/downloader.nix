{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.downloader;
in {
  options.modules.desktop.media.downloader = {
    transmission.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.transmission.enable {
      user = {
        packages = with pkgs; [transmission-gtk];
        extraGroups = ["transmission"];
      };

      networking.firewall = {
        allowedTCPPorts = [51413];
        allowedUDPPorts = [51413];
      };
    })
  ];
}
