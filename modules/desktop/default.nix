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
      type = types.enum [ "x11" "wayland" ];
      description = "The display protocol to use, incl. packages to install";
      default = "x11";
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

    user.packages = with pkgs; [
      hyperfine
      kalker
      (makeDesktopItem {
        name = "kalker";
        desktopName = "Kalker";
        icon = "calc";
        exec = "${getExe wezterm} start kalker";
        categories = [ "Development" ];
      })
      qgnomeplatform
      gucharmap
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [ sarasa-gothic scheherazade-new ];
    };

    services.xserver.displayManager = mkMerge [
      {
        sessionCommands = ''
          # GTK2_RC_FILES must be available to the display manager.
          export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
        '';
      }

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

    env = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
      QT_QPA_PLATFORMTHEME = "gnome";
      # QT_STYLE_OVERRIDE = "kvantum";
    };

    system.userActivationScripts.cleanupHome = ''
      pushd "${config.user.home}"
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';
  };

  # KDE-Connect + Start-up indicator
  programs.kdeconnect.enable = true;

  systemd.user.services.kdeconnect-indicator = {
    serviceConfig.ExecStart = "${pkgs.plasma5Packages.kdeconnect-kde}/bin/kdeconnect-indicator";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
  };
}
