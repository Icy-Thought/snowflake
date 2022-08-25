{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.editor;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.media.editor = {
    raster.enable = mkBoolOpt false;
    vector.enable = mkBoolOpt false;
    modeling.enable = mkBoolOpt false;
  };

  config = {
    user.packages = mkMerge (with pkgs; [
      [
        font-manager
        imagemagick
      ]

      # Illustrator & Indesign replacement:
      (mkIf cfg.vector.enable [
        inkscape
      ])

      # Photoshop replacement:
      (mkIf cfg.raster.enable [
        krita
        gimp
        gimpPlugins.resynthesizer
      ])

      # 3D-Modelling:
      (mkIf cfg.modeling.enable [
        blender
      ])
    ]);

    # TODO: setup GIMP on rebuild!
    # home.configFile = mkIf cfg.raster.enable {
    #   "GIMP/2.10" = {
    #     source = "${configDir}/gimp";
    #     recursive = true;
    #   };
    # };
  };
}
