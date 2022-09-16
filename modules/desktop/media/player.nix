{
  inputs,
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.player;
in {
  options.modules.desktop.media.player = {
    music.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.music.enable {
      hm.imports = [inputs.spicetify-nix.homeManagerModule];

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
      user.packages = with pkgs; [mpv-with-scripts mpvc];
    })
  ];
}
