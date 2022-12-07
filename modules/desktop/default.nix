{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop;
in {
  options.modules.desktop = {
    envProto = mkOption {
      type = with types; nullOr (enum [ "x11" "wayland" ]);
      description = "What display protocol to use";
      default = null;
    };
  };

  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't enable DE/WM < 2 at the same time.";
      }
      {
        assertion =
          let srv = config.services;
          in srv.xserver.enable
            || srv.sway.enable
            || !(anyAttrs
            (n: v: isAttrs v && anyAttrs
              (n: v: isAttrs v && v.enable))
            cfg);
        message = "Can't enable a desktop-app without a DE/WM.";
      }
    ];

    user.packages = with pkgs;
      let myTerm = config.modules.desktop.terminal.default;
      in [
        hyperfine
        gucharmap
        qgnomeplatform # Qt -> GTK Theme
        adwaita-qt # Qt -> GTK Theme
        kalker
        (makeDesktopItem {
          name = "kalker";
          desktopName = "Kalker";
          icon = "calc";
          exec = "${myTerm} start kalker";
          categories = [ "Development" ];
        })
      ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [ sarasa-gothic scheherazade-new ];
    };

    services.xserver.displayManager = mkMerge [
      (mkIf (cfg.envProto == "x11") {
        lightdm.enable = true;
        lightdm.greeters.mini = {
          enable = true;
          user = config.user.name;
        };
      })
      (mkIf (cfg.envProto == "wayland") {
        gdm.enable = true;
        gdm.wayland = true;
      })
    ];

    xdg.portal = {
      enable = true;
      wlr = mkIf (cfg.envProto == "wayland") { enable = true; };
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    env = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
      QT_QPA_PLATFORMTHEME = "gnome";
      QT_STYLE_OVERRIDE = "adwaita";
    };

    system.userActivationScripts.cleanupHome = ''
      pushd "${config.user.home}"
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';

    # Functional `pkgs.light`:
    programs.light.enable = true;

    # KDE-Connect + Start-up indicator
    programs.kdeconnect.enable = true;

    systemd.user.services.kdeconnect-indicator = {
      serviceConfig.ExecStart = "${pkgs.plasma5Packages.kdeconnect-kde}/bin/kdeconnect-indicator";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
    };
  };
}
