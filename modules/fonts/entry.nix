{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let cfg = config.modules.fonts.entry;
in {
  options.modules.fonts.entry = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts = {
      enableDefaultFonts = true;

      fonts = with pkgs; [
        (nerdfonts.override {
          fonts = [ "JetBrainsMono" "FantasqueSansMono" ];
        })
        iosevka
        comfortaa
        source-code-pro
        liberation_ttf
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
      ];

      fontconfig = {
        enable = true;
        defaultFonts = {
          serif = [ "Cantarell" "Noto Kufi Arabic" ];
          sansSerif = [ "Cantarell" "Noto Kufi Arabic" ];
          monospace = [ "Cantarell" "Noto Kufi Arabic" ];
        };
      };
    };
  };
}
