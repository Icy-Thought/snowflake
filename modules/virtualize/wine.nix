{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.virtualisation.wine = {
    enable = mkEnableOption "compatibility layer -> windows.exe";
  };

  config = mkIf config.modules.virtualisation.wine.enable {
    user.packages = with pkgs; [
      bottles
      winetricks
      wineWowPackages.fonts
      wineWowPackages.stagingFull
    ];
  };
}
