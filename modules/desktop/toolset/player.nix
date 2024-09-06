{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.toolset.player;
in {
  options.modules.desktop.toolset.player =
    let inherit (lib.options) mkEnableOption;
    in {
      music.enable = mkEnableOption "music player";
      video.enable = mkEnableOption "video player";
    };

  config = mkMerge [
    (mkIf cfg.music.enable {
      hm.imports = [ inputs.spicetify-nix.homeManagerModules.default ];

      hm.programs.spicetify =
        let spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
        in {
          enable = true;
          theme = spicePkgs.themes.catppuccin;
          colorScheme = "mocha";

          enabledCustomApps =
            attrValues { inherit (spicePkgs.apps) newReleases lyricsPlus; };
          enabledExtensions = attrValues {
            inherit (spicePkgs.extensions)
              adblock fullAppDisplay hidePodcasts keyboardShortcut playNext
              showQueueDuration shuffle;
          };
        };
    })

    (mkIf cfg.video.enable {
      hm.programs.mpv = {
        enable = true;
        scripts = attrValues {
          inherit (pkgs.mpvScripts) autoload mpris sponsorblock thumbnail;
        };
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

      user.packages = [ pkgs.mpvc ];
    })
  ];
}
