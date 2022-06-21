{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't enable DE/WM <2 at the same time.";
      }
      {
        assertion = let
          srv = config.services;
        in
          srv.xserver.enable
          || srv.sway.enable
          || !(anyAttrs
            (n: v: isAttrs v && anyAttrs (n: v: isAttrs v && v.enable))
            cfg);
        message = "Can't enable a desktop-app without a DE/WM.";
      }
    ];

    user.packages = with pkgs; [
      libqalculate
      (makeDesktopItem {
        name = "kalker";
        desktopName = "Qalculate";
        icon = "calc";
        exec = "${getExe kitty} -T Qalculate -e qalc";
        categories = ["Development"];
      })
      qgnomeplatform
      xcolor
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        sarasa-gothic
        scheherazade-new
      ];
    };

    # LightDM
    services.xserver.displayManager.lightdm.greeters.mini.user =
      config.user.name;

    # Try really hard to get QT to respect my GTK theme.
    env = {
      GTK_DATA_PREFIX = ["${config.system.path}"];
      QT_QPA_PLATFORMTHEME = "gnome";
      # QT_STYLE_OVERRIDE = "kvantum";
    };

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
