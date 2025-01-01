{ inputs, options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.toolset.player;
in with lib; {
  options.modules.desktop.toolset.player = {
    music.enable = mkEnableOption "music player";
    video.enable = mkEnableOption "video player";
  };

  config = mkMerge [
    (mkIf cfg.music.enable { user.packages = [ pkgs.youtube-music ]; })

    (mkIf cfg.video.enable {
      hm.programs.mpv = {
        enable = true;
        scripts = with pkgs.mpvScripts; [
          autoload
          mpris
          sponsorblock
          thumbfast
          uosc
        ];
        config = {
          osc = "no";
          profile = "gpu-hq";
          force-window = true;
          save-position-on-quit = "yes";
          ytdl-format = "bestvideo+bestaudio";
          watch-later-dir = "${config.hm.xdg.dataHome}/watch_later";

          sub-font = "Trebuchet MS";
          sub-font-size = 35;
          sub-border-size = 3;
          sub-shadow-offset = 2;
          sub-shadow-color = "0.0/0.0/0.0";
        };
        scriptOpts.autoload.same_type = true;
        bindings = {
          WHEEL_UP = "seek 10";
          WHEEL_DOWN = "seek -10";
        };
      };
    })
  ];
}
