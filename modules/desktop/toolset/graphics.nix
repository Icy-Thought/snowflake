{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.options) mkEnableOption;

  cfg = config.modules.desktop.toolset.graphics;
in {
  options.modules.desktop.toolset.graphics = {
    base.enable = mkEnableOption true;
    modeling.enable = mkEnableOption false;
    raster.enable = mkEnableOption false;
    vector.enable = mkEnableOption false;
  };

  config = {
    user.packages = attrValues ({ } // optionalAttrs cfg.base.enable {
      inherit (pkgs) hyprpicker font-manager imagemagick;
    } // optionalAttrs cfg.vector.enable { inherit (pkgs) inkscape rnote; }
      // optionalAttrs cfg.raster.enable {
        inherit (pkgs) gimp;
        inherit (pkgs.gimpPlugins) resynthesizer;
      } // optionalAttrs cfg.modeling.enable { inherit (pkgs) blender; });
  };
}
