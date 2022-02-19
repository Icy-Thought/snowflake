{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.media.recording;
in {
  options.modules.desktop.appliances.media.recording = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
    # Audio recording + Mastering:
      (if cfg.audio.enable then [ audacity ] else [ ]) ++

      # Streaming + Screen-recodring:
      (if cfg.video.enable then [ obs-studio handbrake ] else [ ]);
  };
}
