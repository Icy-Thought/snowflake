{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.toolset.recorder;
in {
  options.modules.desktop.toolset.recorder = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    services.pipewire.jack.enable = true;

    user.packages = with pkgs;
      [
        ffmpeg
      ]
      # Audio recording + Mastering:
      ++ lists.optionals cfg.audio.enable [
        unstable.audacity-gtk3
        unstable.helvum
      ]
      # Streaming + Screen-recodring:
      ++ lists.optionals cfg.video.enable [
        unstable.obs-studio
        unstable.handbrake
      ];
  };
}
