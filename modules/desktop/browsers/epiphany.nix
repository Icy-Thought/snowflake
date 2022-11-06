{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.browsers.epiphany = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.browsers.epiphany.enable {
    user.packages = with pkgs; [
      epiphany

      (makeDesktopItem {
        name = "epiphany-private";
        desktopName = "Gnome Web Browser (Private)";
        genericName = "Launch a Private Gnome Web Instance";
        icon = "org.gnome.Epiphany";
        exec = "${getExe epiphany} --private-instance";
        categories = [ "Network" "GNOME" "GTK" "WebBrowser" ];
      })
    ];
  };
}
