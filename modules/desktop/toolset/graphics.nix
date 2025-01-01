{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.toolset.graphics;
in with lib; {
  options.modules.desktop.toolset.graphics = {
    base.enable = mkEnableOption "base packages" // { default = true; };
    modeling.enable = mkEnableOption "3D modeling";
    raster.enable = mkEnableOption "rasterized editing";
    vector.enable = mkEnableOption "vectorized editing";
  };

  config = {
    user.packages = with pkgs;
      [ ] ++ optionals cfg.base.enable [
        font-manager
        imagemagick
        upscayl
        (if (config.modules.desktop.type == "wayland") then
          hyprpicker
        else
          xcolor)
      ] ++ optionals cfg.vector.enable [ inkscape rnote ]
      ++ optionals cfg.raster.enable [
        gimp
        # gimpPlugins.resynthesizer
      ] ++ optionals cfg.modeling.enable [ blender ];
  };
}
