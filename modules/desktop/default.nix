{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't enable DE/WM <2 at the same time.";
      }
      {
        assertion = let srv = config.services;
        in srv.xserver.enable || srv.sway.enable || !(anyAttrs
          (n: v: isAttrs v && anyAttrs (n: v: isAttrs v && v.enable)) cfg);
        message = "Can't enable a desktop-app without a DE/WM.";
      }
    ];

    user.packages = with pkgs; [
      bitwarden
      libqalculate
      (makeDesktopItem {
        name = "qalc";
        desktopName = "Qalculator";
        icon = "calc";
        exec = "${alacritty}/bin/alacritty -t Qalculator -e qalc";
        categories = [ "Development" ];
      })
      qgnomeplatform
      libsForQt5.qtstyleplugin-kvantum
    ];

    modules.desktop.appliances.termIce = {
      colorPanes.enable = true;
      htop.enable = true;
      neofetch.enable = true;
    };

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [ scheherazade-new sarasa-gothic ];
    };

    # Try really hard to get QT to respect my GTK theme.
    env.GTK_DATA_PREFIX = [ "${config.system.path}" ];
    env.QT_QPA_PLATFORMTHEME = "gnome";
    env.QT_STYLE_OVERRIDE = "kvantum";

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
    '';

    # Take care of the garbage:
    system.userActivationScripts.cleanupHome = ''
      pushd "${config.user.home}"
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';
  };
}
