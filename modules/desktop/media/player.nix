{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.player;
  themeCfg = config.modules.themes;
in {
  options.modules.desktop.media.player = {
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.music.enable {
      user.packages = with pkgs; [spotify];
      # TODO: spicetify-cli + activeTheme.
    })

    (mkIf cfg.video.enable {
      user.packages = with pkgs; [mpv-with-scripts mpvc];
    })
  ];
}
