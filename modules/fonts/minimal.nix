{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let
  cfg = config.modules.fonts.minimal;
  font = config.modules.themes.font;
in {
  options.modules.fonts.minimal = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts = {
      enableDefaultFonts = true;

      fonts = with pkgs; [ noto-fonts noto-fonts-emoji source-code-pro ];

      fontconfig = {
        enable = true;
        defaultFonts = {
          serif = [ "${font}" "Noto Kufi Arabic" ];
          sansSerif = [ "${font}" "Noto Kufi Arabic" ];
          monospace = [ "${font}" "Noto Kufi Arabic" ];
        };
      };
    };
  };
}
