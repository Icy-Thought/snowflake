{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) isAttrs;
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.my) anyAttrs countAttrs value;

  cfg = config.modules.desktop;
in {
  options.modules.desktop = let
    inherit (lib.types) either str;
    inherit (lib.my) mkOpt;
  in {
    type = mkOpt (either str null) null;
  };

  config = mkMerge [
    {
      assertions = let
        isEnabled = _: v: v.enable or false;
        hasDesktopEnabled = cfg:
          (anyAttrs isEnabled cfg)
          || !(anyAttrs (_: v: isAttrs v && anyAttrs isEnabled v) cfg);
      in [
        {
          assertion =
            (countAttrs (_: v: v.enable or false) cfg) < 2;
          message = "Can't have more than one desktop environment enabled at a time";
        }
        {
          assertion = hasDesktopEnabled cfg;
          message = "Can't enable a desktop sub-module without a desktop environment";
        }
        {
          assertion = !(hasDesktopEnabled cfg) || cfg.type != null;
          message = "Downstream desktop module did not set modules.desktop.type!";
        }
      ];
    }

    (mkIf (cfg.type != null) {
      env.GTK_DATA_PREFIX = ["${config.system.path}"];

      system.userActivationScripts.cleanupHome = ''
        pushd "${config.user.home}"
        rm -rf .compose-cache .nv .pki .dbus .fehbg
        [ -s .xsession-errors ] || rm -f .xsession-errors*
        popd
      '';

      user.packages = attrValues {
        inherit
          (pkgs)
          nvfetcher
          clipboard-jh
          gucharmap
          hyperfine
          kalker
          ;

        kalker-launcher = pkgs.makeDesktopItem {
          name = "Kalker";
          desktopName = "Kalker";
          icon = "calc";
          exec = "${config.modules.desktop.terminal.default} start kalker";
          categories = ["Education" "Science" "Math"];
        };
      };

      services.xserver.enable = true;
      xdg.portal = {
        enable = true;
        extraPortals = [pkgs.xdg-desktop-portal-gtk];
        config.common.default = "*";
      };
      services.gnome.gnome-keyring.enable = true;

      fonts = {
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        packages = attrValues {
          inherit (pkgs) sarasa-gothic scheherazade-new;
        };
      };

      hm.qt = {
        enable = true;
        platformTheme.name = "adwaita";
        style.name = "adwaita-dark";
      };
    })

    (mkIf (cfg.type == "wayland") {
      xdg.portal.wlr.enable = true;

      # Login Manager: ReGreet!
      programs.regreet.enable = true;
    })

    (mkIf (cfg.type == "x11") {
      security.pam.services = {
        login.enableGnomeKeyring = true;
        lightdm.enableGnomeKeyring = true;
      };

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
