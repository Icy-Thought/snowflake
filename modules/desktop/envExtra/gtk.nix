{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envExtra.gtk;
  font = config.modules.fonts.settings;
in {
  options.modules.desktop.envExtra.gtk = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager = {
      gtk = {
        enable = true;
        font = {
          name = "${font.family}";
          size = 10;
        };

        iconTheme.name = "WhiteSur-dark";
        iconTheme.package = pkgs.whitesur-icon-theme;

        theme.name = "Orchis-dark-compact";
        theme.package = pkgs.orchis-theme;
      };

      # QT -> GTK
      qt = {
        enable = true;
        platformTheme = "gtk";
        style.name = "gtk2";
      };
    };
  };
}
