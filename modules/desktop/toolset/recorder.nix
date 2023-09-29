{
  config,
  options,
  lib,
  pkgs,
  ...
}: {
  options.modules.desktop.toolset.recorder = let
    inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption false;
    audio.enable = mkEnableOption "audio manipulation" // {default = true;};
    video.enable = mkEnableOption "video manipulation" // {default = true;};
  };

  config = let
    inherit (lib.attrsets) attrValues optionalAttrs;
    inherit (lib.modules) mkIf;

    cfg = config.modules.desktop.toolset.recorder;
  in
    mkIf cfg.enable {
      services.pipewire.jack.enable = true;

      user.packages = attrValues ({
          inherit (pkgs) ffmpeg;
        }
        // optionalAttrs cfg.audio.enable {
          inherit (pkgs.unstable) audacity-gtk3 helvum;
        }
        // optionalAttrs cfg.video.enable {
          inherit (pkgs.unstable) obs-studio handbrake;
        });
    };
}
