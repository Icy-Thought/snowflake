{ options, config, lib, pkgs, ... }: {

  options.modules.desktop.toolset.recorder =
    let inherit (lib.options) mkEnableOption;
    in {
      enable = mkEnableOption false;
      audio.enable = mkEnableOption "audio manipulation";
      video.enable = mkEnableOption "video manipulation";
    };

  config = let
    inherit (lib.attrsets) attrValues optionalAttrs;
    inherit (lib.modules) mkIf;

    cfg = config.modules.desktop.toolset.recorder;
  in mkIf cfg.enable {
    services.pipewire.jack.enable = true;

    user.packages = attrValues ({
      inherit (pkgs) ffmpeg-full;
    } // optionalAttrs cfg.audio.enable {
      inherit (pkgs.unstable) audacity helvum;
    } // optionalAttrs cfg.video.enable {
      inherit (pkgs.unstable) obs-studio;
    });
  };
}
