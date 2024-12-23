{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.virtualisation.wine =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "compatibility layer -> windows.exe"; };

  config = mkIf config.modules.virtualisation.wine.enable {
    user.packages = attrValues {
      inherit (pkgs) bottles winetricks;
      inherit (pkgs.wineWowPackages) fonts stagingFull;
    };
  };
}
