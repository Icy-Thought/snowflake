{ options, config, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.virtual.wine = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.virtual.wine.enable {
    user.packages = attrValues ({
      inherit (pkgs) bottles winetricks;
      inherit (pkgs.wineWowPackages) fonts stagingFull;
    });
  };
}
