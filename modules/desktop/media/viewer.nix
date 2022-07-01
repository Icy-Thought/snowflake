{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.viewer;

  # Theme-related let's
  active = config.modules.themes.active;
  colors = config.modules.themes.colors;
  font = config.modules.themes.font;
in {
  options.modules.desktop.media.viewer = {
    document.enable = mkBoolOpt false;
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.document.enable {
      home.programs.zathura.enable = true;
    })

    (mkIf cfg.music.enable {
      user.packages = with pkgs; [spotify];
      # TODO: spicetify-cli + activeTheme.
    })

    (mkIf cfg.video.enable {
      user.packages = with pkgs; [mpv-with-scripts mpvc];
    })
  ];
}
