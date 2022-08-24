{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.recorder;
in {
  options.modules.desktop.media.recorder = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    services.pipewire.jack.enable = true;

    user.packages = [
      # Audio recording + Mastering:
      (
        mkIf (cfg.audio.enable)
        pkgs.unstable.audacity-gtk3
        pkgs.unstable.helvum
      )

      # Streaming + Screen-recodring:
      (
        mkIf (cfg.video.enable)
        pkgs.unstable.obs-studio
        pkgs.unstable.handbrake
      )
    ];
  };
}
