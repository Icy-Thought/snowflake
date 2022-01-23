{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let cfg = config.modules.fonts.nerdFonts;
in {
  options.modules.fonts.nerdFonts = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs;
      [
        (nerdfonts.override { fonts = [ "VictorMono" "FantasqueSansMono" ]; })
      ];
  };
}
