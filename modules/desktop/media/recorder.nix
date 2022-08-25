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

    user.packages = mkMerge (with pkgs; [
      # Audio recording + Mastering:
      (mkIf cfg.audio.enable [
        unstable.audacity-gtk3
        unstable.helvum
      ])

      # Streaming + Screen-recodring:
      (mkIf cfg.video.enable [
        unstable.obs-studio
        unstable.handbrake
      ])
    ]);
  };
}
