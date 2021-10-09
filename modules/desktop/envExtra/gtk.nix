{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envExtra.gtk;
in {
  options.modules.desktop.envExtra.gtk = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    gtk.enable = true;
    gtk.font.name = "JetBrainsMonoMedium Nerd Font";
    gtk.font.size = 10;

    gtk.iconTheme.name = "WhiteSur-dark";
    gtk.iconTheme.package = pkgs.whitesur-icon-theme;

    gtk.theme.name = "Orchis-dark-compact";
    gtk.theme.package = pkgs.orchis-theme;

    # QT -> GTK
    qt.enable = true;
    qt.platformTheme = "gtk";
    qt.style.name = "gtk2";
  };
}
