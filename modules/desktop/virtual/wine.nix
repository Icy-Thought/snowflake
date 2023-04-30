{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.virtual.wine =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

  config = mkIf config.modules.desktop.virtual.wine.enable {
    user.packages = attrValues ({
      inherit (pkgs) bottles winetricks;
      inherit (pkgs.wineWowPackages) fonts stagingFull;
    });
  };
}
