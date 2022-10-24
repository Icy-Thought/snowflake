{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.graphics;
in {
  options.modules.desktop.toolset.graphics = {
    base.enable = mkBoolOpt true;
    modeling.enable = mkBoolOpt false;
    raster.enable = mkBoolOpt false;
    vector.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.base.enable {
      user.packages = with pkgs; [
        eyedropper
        font-manager
        imagemagick
      ];
    })

    # Illustrator & Indesign replacement:
    (mkIf cfg.vector.enable {
      user.packages = with pkgs; [ inkscape rnote ];
      # TODO: hard-coded inkscape config
    })

    # Photoshop replacement:
    (mkIf cfg.raster.enable {
      user.packages = with pkgs; [
        # krita
        gimp
        gimpPlugins.resynthesizer
      ];

      # home.configFile."GIMP/2.10" = {
      #   source = "${configDir}/gimp";
      #   recursive = true;
      # };
    })

    # 3D-Modelling
    (mkIf cfg.modeling.enable {
      user.packages = with pkgs; [ blender ];
    })
  ];
}
