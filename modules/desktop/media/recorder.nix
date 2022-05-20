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

    user.packages = with pkgs;
    # Audio recording + Mastering:
      (
        if cfg.audio.enable
        then [
          unstable.audacity-gtk3
          unstable.helvum
        ]
        else []
      )
      ++
      # Streaming + Screen-recodring:
      (
        if cfg.video.enable
        then [
          unstable.obs-studio
          unstable.handbrake
        ]
        else []
      );
  };
}
