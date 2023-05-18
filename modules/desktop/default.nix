{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) isAttrs;
  inherit (lib) attrValues mkIf mkMerge mkOption;
  inherit (lib.types) nullOr enum;
  inherit (lib.my) anyAttrs countAttrs value;

  cfg = config.modules.desktop;
in {
  options.modules.desktop = {
    envProto = mkOption {
      type = nullOr (enum [ "x11" "wayland" ]);
      description = "What display protocol to use.";
      default = null;
    };
  };

  config = mkMerge [
    {
      assertions = [
        {
          assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
          message = "Prevent DE/WM > 1 from being enabled.";
        }
        {
          assertion = let srv = config.services;
          in srv.xserver.enable || srv.sway.enable || !(anyAttrs
            (n: v: isAttrs v && anyAttrs (n: v: isAttrs v && v.enable)) cfg);
          message =
            "Prevent desktop applications from enabling without a DE/WM.";
        }
      ];

      env = {
        GTK_DATA_PREFIX = [ "${config.system.path}" ];
        QT_QPA_PLATFORMTHEME = "gnome";
        QT_STYLE_OVERRIDE = "Adwaita";
      };

      system.userActivationScripts.cleanupHome = ''
        pushd "${config.user.home}"
        rm -rf .compose-cache .nv .pki .dbus .fehbg
        [ -s .xsession-errors ] || rm -f .xsession-errors*
        popd
      '';

      user.packages = attrValues ({
        inherit (pkgs)
          clipboard-jh gucharmap hyperfine kalker
          qgnomeplatform; # Qt -> GTK Theme

        kalker-launcher = pkgs.makeDesktopItem {
          name = "Kalker";
          desktopName = "Kalker";
          icon = "calc";
          exec = "${config.modules.desktop.terminal.default} start kalker";
          categories = [ "Education" "Science" "Math" ];
        };
      });

      fonts = {
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        fonts = attrValues ({ inherit (pkgs) sarasa-gothic scheherazade-new; });
      };

      # Enabling xserver + x-related settings:
      services.xserver.enable = true;

      xdg.portal = {
        enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };

      # Retain secrets inside Gnome Keyring
      services.gnome.gnome-keyring.enable = true;
      security.pam.services.login.enableGnomeKeyring = true;

      # GUI for our gnome-keyring:
      programs.seahorse.enable = true;
    }

    (mkIf (cfg.envProto == "wayland") {
      xdg.portal.wlr.enable = true;

      # Login Manager: ReGreet!
      programs.regreet.enable = true;
    })

    (mkIf (cfg.envProto == "x11") {
      services.xserver.displayManager.lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          user = config.user.name;
        };
      };

      hm.xsession = {
        enable = true;
        numlock.enable = true;
        preferStatusNotifierItems = true;
      };
    })
  ];
}
