{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.virtual.wine = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.virtual.wine.enable {
    user.packages = with pkgs; [
      bottles
      wineWowPackages.fonts
      wineWowPackages.stagingFull
      winetricks
    ];
  };
}
