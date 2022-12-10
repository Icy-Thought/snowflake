{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.player;
in {
  options.modules.desktop.toolset.player = {
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.music.enable {
      hm.imports = [ inputs.spicetify-nix.homeManagerModules.default ];

      hm.programs.spicetify = {
        enable = true;
        spotifyPackage = pkgs.spotify-unwrapped;
        spicetifyPackage = pkgs.spicetify-cli;
        enabledExtensions = [
          "fullAppDisplay.js"
          "shuffle+.js"
          "adblock.js"
          "hidePodcasts.js"
        ];
        theme = "catppuccin-mocha";
        colorScheme = "flamingo";
      };
    })

    (mkIf cfg.video.enable {
      hm.programs.mpv = {
        enable = true;
        scripts = with pkgs.mpvScripts; [
          autoload
          mpris
          sponsorblock
          thumbnail
        ];
        config = {
          profile = "gpu-hq";
          force-window = true;
          ytdl-format = "bestvideo+bestaudio";
          cache-default = 4000000;
          osc = "no"; # Thumbnail
        };
        bindings = {
          WHEEL_UP = "seek 10";
          WHEEL_DOWN = "seek -10";
        };
      };

      user.packages = with pkgs; [ mpvc ];
    })
  ];
}
