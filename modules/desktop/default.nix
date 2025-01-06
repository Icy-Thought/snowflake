{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop;
in with lib; {
  options.modules.desktop = with types; {
    type = my.mkOpt (either str null) null;
  };

  config = mkMerge [
    {
      assertions = let
        isEnabled = _: v: v.enable or false;
        hasDesktopEnabled = cfg:
          (my.anyAttrs isEnabled cfg)
          || !(my.anyAttrs (_: v: builtins.isAttrs v && my.anyAttrs isEnabled v)
            cfg);
      in [
        {
          assertion = (my.countAttrs (_: v: v.enable or false) cfg) < 2;
          message =
            "Can't have more than one desktop environment enabled at a time";
        }
        {
          assertion = hasDesktopEnabled cfg;
          message =
            "Can't enable a desktop sub-module without a desktop environment";
        }
        {
          assertion = !(hasDesktopEnabled cfg) || cfg.type != null;
          message =
            "Downstream desktop module did not set modules.desktop.type!";
        }
      ];
    }

    (mkIf (cfg.type != null) {
      home.sessionVariables.GTK_DATA_PREFIX = "${config.system.path}";

      system.userActivationScripts.cleanupHome = ''
        pushd "${config.user.home}"
        rm -rf .compose-cache .nv .pki .dbus .fehbg
        [ -s .xsession-errors ] || rm -f .xsession-errors*
        popd
      '';

      user.packages = with pkgs; [
        nvfetcher
        clipboard-jh
        gucharmap
        hyperfine
        kalker

        (makeDesktopItem {
          name = "Kalker";
          desktopName = "Kalker";
          icon = "calc";
          exec = let term = config.modules.desktop.terminal.default;
          in if term == "wezterm" then
            "${term} startx kalker"
          else
            "${term} --class calculator -e kalker";
          categories = [ "Education" "Science" "Math" ];
        })
      ];

      services.xserver.enable = true;
      xdg.portal = {
        enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
        config.common.default = "*";
      };

      programs.regreet = {
        enable = true;
        settings = {
          env.SESSION_DIRS = let
            sessionHome =
              "${config.services.displayManager.sessionData.desktops}/share";
          in concatStringsSep ":" [
            "${sessionHome}/xsessions"
            "${sessionHome}/wayland-sessions"
          ];
          commands.x11_prefix = [ "startx" "/usr/bin/env" ">/dev/null 2>&1" ];
        };
      };

      security.pam.services.greetd.enableGnomeKeyring = true;

      fonts = {
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        packages = with pkgs; [ sarasa-gothic scheherazade-new ];
      };

      hm.qt = {
        enable = true;
        platformTheme.name = "adwaita";
        style.name = "adwaita-dark";
      };
    })

    (mkIf (cfg.type == "wayland") { xdg.portal.wlr.enable = true; })

    (mkIf (cfg.type == "x11") {
      services.xserver.displayManager = {
        startx.enable = true;
        sessionCommands =
          map (cmd: "${getExe pkgs.xorg.xset} ${cmd}") [ "-dpms" "s off" ];
      };

      hm.xsession = {
        enable = true;
        numlock.enable = true;
        preferStatusNotifierItems = true;
      };
    })
  ];
}
