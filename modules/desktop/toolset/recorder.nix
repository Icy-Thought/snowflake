{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues optionalAttrs mkIf;
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

    user.packages = attrValues ({
      inherit (pkgs) ffmpeg;
    } // optionalAttrs cfg.audio.enable {
      inherit (pkgs.unstable) audacity-gtk3 helvum;
    } // optionalAttrs cfg.video.enable {
      inherit (pkgs.unstable) obs-studio handbrake;
    });
  };
}
