{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.virtual.wine = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.virtual.wine.enable {
    user.packages = with pkgs; [
      bottles
      wineWowPackages.fonts
      wineWowPackages.staging
      winetricks
    ];
  };
}
