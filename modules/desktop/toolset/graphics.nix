{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues optionalAttrs;
  cfg = config.modules.desktop.toolset.graphics;
in {
  options.modules.desktop.toolset.graphics =
    let inherit (lib.options) mkEnableOption;
    in {
      base.enable = mkEnableOption "base packages" // { default = true; };
      modeling.enable = mkEnableOption "3D modeling";
      raster.enable = mkEnableOption "rasterized editing";
      vector.enable = mkEnableOption "vectorized editing";
    };

  config = {
    user.packages = let desktop = config.modules.desktop;
    in attrValues ({ } // optionalAttrs cfg.base.enable {
      inherit (pkgs) font-manager imagemagick upscayl;
      colorPicker =
        if (desktop.type == "wayland") then pkgs.hyprpicker else pkgs.xcolor;
    } // optionalAttrs cfg.vector.enable { inherit (pkgs) inkscape rnote; }
      // optionalAttrs cfg.raster.enable {
        inherit (pkgs) gimp;
        # inherit (pkgs.gimpPlugins) resynthesizer;
      } // optionalAttrs cfg.modeling.enable { inherit (pkgs) blender; });
  };
}
