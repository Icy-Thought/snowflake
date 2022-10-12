{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.recorder;
in {
  options.modules.desktop.toolset.recorder = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    services.pipewire.jack.enable = true;

    user.packages = with pkgs; [
      # Audio recording + Mastering:
      (mkIf (cfg.audio.enable)
        unstable.audacity-gtk3
        unstable.helvum
      )

      # Streaming + Screen-recodring:
      (mkIf (cfg.video.enable)
        unstable.obs-studio
        unstable.handbrake
      )
    ];
  };
}
