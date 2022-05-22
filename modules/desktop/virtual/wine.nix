{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.virtual.wine;
in {
  options.modules.desktop.virtual.wine = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      wineWowPackages.fonts
      wineWowPackages.staging
      winetricks
    ];
  };
}
