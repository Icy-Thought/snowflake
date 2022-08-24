{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gaming.lutris;
  wineCfg = config.modules.desktop.virtual.wine;
in {
  options.modules.desktop.gaming.lutris = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      networking.firewall.allowedTCPPorts = [443]; # League of Legends (OpenSSL)
    }

    (mkIf (cfg.enable && wineCfg.enable) {
      user.packages = [pkgs.lutris];
    })

    (mkIf (cfg.enable && !wineCfg.enable) {
      user.packages = [
        pkgs.lutris
        pkgs.wineWowPackages.fonts
        pkgs.wineWowPackages.staging
        pkgs.winetricks
      ];
    })
  ];
}
