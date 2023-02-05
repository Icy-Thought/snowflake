{ inputs, options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.toolset.player;
in {
  options.modules.desktop.toolset.player = {
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.music.enable {
      hm.imports = [ inputs.spicetify-nix.homeManagerModules.default ];

      hm.programs.spicetify =
        let spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
        in {
          enable = true;
          spotifyPackage = pkgs.spotify-unwrapped;
          spicetifyPackage = pkgs.spicetify-cli;

          theme = spicePkgs.themes.catppuccin-mocha;
          colorScheme = "flamingo";

          enabledCustomApps = with spicePkgs.apps; [ new-releases lyrics-plus ];
          enabledExtensions = with spicePkgs.extensions; [
            adblock
            fullAppDisplay
            hidePodcasts
            keyboardShortcut
            playNext
            showQueueDuration
            shuffle
          ];
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
