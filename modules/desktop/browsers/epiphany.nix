{ options, config, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.desktop.browsers.epiphany =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "WebKit based browser"; };

  config = mkIf config.modules.desktop.browsers.epiphany.enable {
    user.packages = let
      inherit (pkgs) makeDesktopItem epiphany;

      desktopItem = makeDesktopItem {
        name = "epiphany";
        desktopName = "Gnome Web Browser";
        genericName = "Launch a Gnome Web Instance";
        icon = "org.gnome.Epiphany";
        exec = "epiphany --new-window %U";
        categories = [ "Network" "GNOME" "GTK" "WebBrowser" ];
      };

      privDesktopItem = makeDesktopItem {
        name = "epiphany-private";
        desktopName = "Gnome Web Browser (Private)";
        genericName = "Launch a Private Gnome Web Instance";
        icon = "org.gnome.Epiphany";
        exec = "epiphany -p --new-window %U";
        categories = [ "Network" "GNOME" "GTK" "WebBrowser" ];
      };

      epiphany' = pkgs.symlinkJoin {
        name = "epiphany-with-desktopItem";
        paths = [ epiphany desktopItem privDesktopItem ];
        postBuild = ''
          rm $out/share/applications/*
          cp ${desktopItem}/share/applications/* $out/share/applications/
          cp ${privDesktopItem}/share/applications/* $out/share/applications/
        '';
      };
    in [ epiphany' ];
  };
}
