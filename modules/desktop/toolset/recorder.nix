{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.toolset.recorder = {
    enable = mkEnableOption false;
    audio.enable = mkEnableOption "audio manipulation";
    video.enable = mkEnableOption "video manipulation";
  };

  config = let cfg = config.modules.desktop.toolset.recorder;
  in mkIf cfg.enable {
    services.pipewire.jack.enable = true;

    user.packages = with pkgs;
      [ ffmpeg-full ]
      ++ optionals cfg.audio.enable [ unstable.audacity unstable.helvum ]
      ++ optionals cfg.video.enable [ unstable.obs-studio ];
  };
}
