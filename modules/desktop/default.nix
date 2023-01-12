{ config
, options
, lib
, pkgs
, ...
}:

let
  inherit (builtins) isAttrs;
  inherit (lib) mkIf mkMerge mkOption;
  inherit (lib.types) nullOr enum;
  inherit (lib.my) anyAttrs countAttrs value;

  cfg = config.modules.desktop;
in
{
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
          assertion =
            let srv = config.services;
            in srv.xserver.enable
              || srv.sway.enable
              || !(anyAttrs
              (n: v: isAttrs v && anyAttrs
                (n: v: isAttrs v && v.enable))
              cfg);
          message = "Prevent desktop applications from enabling without a DE/WM.";
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

      user.packages = with pkgs;
        let myTerm = config.modules.desktop.terminal.default;
        in [
          hyperfine
          gucharmap
          qgnomeplatform # Qt -> GTK Theme
          libqalculate
          (makeDesktopItem {
            name = "Qalc";
            desktopName = "Qalc";
            icon = "calc";
            exec = "${myTerm} start qalc";
            categories = [ "Education" "Science" "Math" ];
          })
        ];

      fonts = {
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        fonts = with pkgs; [ sarasa-gothic scheherazade-new ];
      };

      xdg.portal = {
        enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };

      # Retain secrets inside Gnome Keyring
      services.gnome.gnome-keyring.enable = true;

      # Functional `pkgs.light` for `/bin/brightctl`
      programs.light.enable = true;

      # KDE-Connect + Start-up indicator
      programs.kdeconnect.enable = true;

      systemd.user.services.kdeconnect-indicator = {
        serviceConfig.ExecStart = "${pkgs.plasma5Packages.kdeconnect-kde}/bin/kdeconnect-indicator";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
      };
    }

    (mkIf (cfg.envProto == "x11") {
      services.xserver = {
        enable = true;
        displayManager.lightdm = {
          enable = true;
          greeters.mini = {
            enable = true;
            user = config.user.name;
          };
        };
      };

      hm.xsession = {
        enable = true;
        numlock.enable = true;
        preferStatusNotifierItems = true;
      };
    })

    (mkIf (cfg.envProto == "wayland") {
      xdg.portal.wlr.enable = true;

      services.xserver = {
        enable = true;
        displayManager.gdm = {
          enable = true;
          wayland = true;
        };
      };
    })
  ];
}
