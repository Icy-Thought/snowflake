{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues optionalAttrs;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.toolset.graphics;
in {
  options.modules.desktop.toolset.graphics = {
    base.enable = mkBoolOpt true;
    modeling.enable = mkBoolOpt false;
    raster.enable = mkBoolOpt false;
    vector.enable = mkBoolOpt false;
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
