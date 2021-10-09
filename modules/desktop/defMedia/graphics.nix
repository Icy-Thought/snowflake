{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.defMedia.graphics;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.defMedia.graphics = {
    enable = mkBoolOpt false;
    tools.enable = mkBoolOpt true;
    raster.enable = mkBoolOpt true;
    vector.enable = mkBoolOpt true;
    modeling.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (if cfg.tools.enable then [ font-manager imagemagick ] else [ ]) ++

      # Illustrator & Indesign replacement:
      (if cfg.vector.enable then [ inkscape ] else [ ]) ++

      # Photoshop replacement:
      (if cfg.raster.enable then [
        krita
        gimp
        gimpPlugins.resynthesizer
      ] else
        [ ]) ++

      # 3D-Modelling:
      (if cfg.modeling.enable then [ blender ] else [ ]);

    # TODO: gimp setup on rebuild!
    # home.configFile = mkIf cfg.raster.enable {
    #   "GIMP/2.10" = {
    #     source = "${configDir}/gimp";
    #     recursive = true;
    #   };
    # };
  };
}
