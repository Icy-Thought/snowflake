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
in {
  options.modules.desktop.media.viewer = {
    document.enable = mkBoolOpt false;
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.document.enable {
      user.packages = with pkgs; [zathura];
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
