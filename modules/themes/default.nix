{ options, config, lib, pkgs, ... }:

let
  cfg = config.modules.themes;
  desktop = config.modules.desktop;
in with lib; {
  options.modules.themes = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v:
        let theme = builtins.getEnv "THEME";
        in if theme != "" then theme else v;
      description = ''
        Name of the theme which ought to be applied.
        Can be overridden by the `THEME` environment variable.
      '';
    };

    wallpaper = my.mkOpt (nullOr path) null;

    loginWallpaper = my.mkOpt (nullOr path) (if cfg.wallpaper != null then
      my.toFilteredImage cfg.wallpaper
      "-gaussian-blur 0x2 -modulate 70 -level 5%"
    else
      null);

    gtk = {
      name = my.mkOpt str "";
      package = mkPackageOption pkgs "gtk" { };
    };

    iconTheme = {
      name = my.mkOpt str "";
      package = mkPackageOption pkgs "icon" { };
    };

    pointer = {
      name = my.mkOpt str "";
      package = mkPackageOption pkgs "pointer" { };
      size = my.mkOpt int 24;
    };

    onReload = my.mkOpt (attrsOf lines) { };

    fontConfig = {
      packages = my.mkOpt (listOf package) [ ];
      mono = my.mkOpt (listOf str) [ "" ];
      sans = my.mkOpt (listOf str) [ "" ];
      emoji = my.mkOpt (listOf str) [ "" ];
    };

    font = {
      mono = {
        family = my.mkOpt str "";
        weight = my.mkOpt str "Bold";
        weightAlt = my.mkOpt str "Bold";
        weightNum = my.mkOpt int 700;
        size = my.mkOpt int 13;
      };
      sans = {
        family = my.mkOpt str "";
        weight = my.mkOpt str "SemiBold";
        weightAlt = my.mkOpt str "DemiBold";
        weightNum = my.mkOpt int 600;
        size = my.mkOpt int 10;
      };
    };

    colors = {
      main = {
        normal = {
          black = my.mkOpt str "#000000"; # 0
          red = my.mkOpt str "#FF0000"; # 1
          green = my.mkOpt str "#00FF00"; # 2
          yellow = my.mkOpt str "#FFFF00"; # 3
          blue = my.mkOpt str "#0000FF"; # 4
          magenta = my.mkOpt str "#FF00FF"; # 5
          cyan = my.mkOpt str "#00FFFF"; # 6
          white = my.mkOpt str "#BBBBBB"; # 7
        };
        bright = {
          black = my.mkOpt str "#888888"; # 8
          red = my.mkOpt str "#FF8800"; # 9
          green = my.mkOpt str "#00FF80"; # 10
          yellow = my.mkOpt str "#FF8800"; # 11
          blue = my.mkOpt str "#0088FF"; # 12
          magenta = my.mkOpt str "#FF88FF"; # 13
          cyan = my.mkOpt str "#88FFFF"; # 14
          white = my.mkOpt str "#FFFFFF"; # 15
        };
        types = with cfg.colors.main; {
          bg = my.mkOpt str normal.black;
          fg = my.mkOpt str normal.white;
          panelbg = my.mkOpt str types.bg;
          panelfg = my.mkOpt str types.fg;
          border = my.mkOpt str types.bg;
          error = my.mkOpt str normal.red;
          warning = my.mkOpt str normal.yellow;
          highlight = my.mkOpt str normal.white;
        };
      };

      rofi = {
        bg = {
          main = my.mkOpt str "#FFFFFF";
          alt = my.mkOpt str "#FFFFFF";
          bar = my.mkOpt str "#FFFFFF";
        };
        fg = my.mkOpt str "#FFFFFF";
        ribbon = {
          outer = my.mkOpt str "#FFFFFF";
          inner = my.mkOpt str "#FFFFFF";
        };
        selected = my.mkOpt str "#FFFFFF";
        urgent = my.mkOpt str "#FFFFFF";
        transparent = my.mkOpt str "#FFFFFF";
      };
    };

    editor = {
      helix = {
        light = my.mkOpt str "";
        dark = my.mkOpt str "";
      };
      neovim = {
        light = my.mkOpt str "";
        dark = my.mkOpt str "";
      };
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    {
      # Allow HM to control GTK Theme:
      programs.dconf.enable = true;

      hm.gtk = {
        enable = true;
        font = with cfg.font.sans; {
          name = family;
          size = size;
        };
        theme = with cfg.gtk; {
          name = name;
          package = package;
        };
        iconTheme = with cfg.iconTheme; {
          name = name;
          package = package;
        };
        gtk3.bookmarks =
          builtins.map (dir: "file://${config.user.home}/" + dir) [
            "Workspace/public/snowflake"
            "Workspace/public/cs-notes"
            "Workspace/public/notebook"
            "Library/unexplored"
            "Library/unexplored/mathematics"
            "Library/unexplored/programming"
          ];
        gtk4.extraConfig = {
          gtk-cursor-blink = false;
          gtk-recent-files-limit = 20;
        };
      };

      home.pointerCursor = with cfg.pointer; {
        name = name;
        package = package;
        size = size;
        gtk.enable = true;
      };

      fonts = with cfg.fontConfig; {
        packages = packages;
        fontconfig.defaultFonts = {
          monospace = mono;
          sansSerif = sans;
          emoji = emoji;
        };
      };

      create.dataFile =
        mkIf (cfg.wallpaper != null) { "wallpaper".source = cfg.wallpaper; };
    }

    (mkIf (desktop.type == "wayland") (mkMerge [
      {
        programs.regreet = {
          theme = with cfg.gtk; {
            name = "${gtk.name}";
            package = "${gtk.package}";
          };
          iconTheme = with cfg.iconTheme; {
            name = "${iconTheme.name}";
            package = "${iconTheme.package}";
          };
          cursorTheme = with cfg.pointer; {
            name = "${pointer.name}";
            package = "${pointer.package}";
          };
          font = with cfg.font; {
            name = "${font.mono.family}";
            size = font.mono.size;
          };
          settings.background = mkIf (cfg.loginWallpaper != null) {
            path = cfg.loginWallpaper;
            fit = "Cover";
          };
        };
      }

      (mkIf (cfg.wallpaper != null) {
        hm.services.hyprpaper = {
          enable = true;
          settings = {
            ipc = "on";
            splash = false;
            splash_offset = 2.0;
          } // mkIf (cfg.wallpaper != null) {
            preload = [ "${builtins.toPath cfg.wallpaper}" ];
            wallpaper = [ "eDP-1,${builtins.toPath cfg.wallpaper}" ];
          };
        };
      })
    ]))

    (mkIf (desktop.type == "x11") (mkMerge [
      {
        hm.xresources = {
          path = "${config.user.home}/.Xresources";
          properties = with cfg.colors.main; {
            "*.foreground" = "${types.fg}";
            "*.background" = "${types.bg}";

            "*.color0" = "${normal.black}";
            "*.color8" = "${bright.black}";

            "*.color1" = "${normal.red}";
            "*.color9" = "${bright.red}";

            "*.color2" = "${normal.green}";
            "*.color10" = "${bright.green}";

            "*.color3" = "${normal.yellow}";
            "*.color11" = "${bright.yellow}";

            "*.color4" = "${normal.blue}";
            "*.color12" = "${bright.blue}";

            "*.color5" = "${normal.magenta}";
            "*.color13" = "${bright.magenta}";

            "*.color6" = "${normal.cyan}";
            "*.color14" = "${bright.cyan}";

            "*.color7" = "${normal.white}";
            "*.color15" = "${bright.white}";
          };
        };

        home.pointerCursor.x11 = {
          enable = true;
          defaultCursor = "left_ptr";
        };
      }

      # Apply theme options -> lightdm-mini-greeter
      (mkIf (cfg.loginWallpaper != null) {
        services.xserver.displayManager.lightdm = {
          greeters.mini.extraConfig = with cfg.colors.main; ''
            background-image = "${cfg.loginWallpaper}"
            background-image-size = "100% 100%"

            text-color = "${types.bg}"
            password-background-color = "${normal.black}"
            window-color = "${types.border}"
            border-color = "${types.border}"
          '';
        };
      })

      # Auto-set wallpaper to prevent $HOME pollution!
      (mkIf (cfg.wallpaper != null) (let
        wCfg = config.services.xserver.desktopManager.wallpaper;
        command = ''
          if [ -e "$XDG_DATA_HOME/wallpaper" ]; then
            ${getExe pkgs.feh} --bg-${wCfg.mode} \
            ${optionalString wCfg.combineScreens "--no-xinerama"} \
            --no-fehbg \
            $XDG_DATA_HOME/wallpaper
          fi
        '';
      in {
        modules.themes.onReload.wallpaper = command;
        services.xserver.displayManager.sessionCommands = command;
      }))

      (mkIf (cfg.onReload != { }) (let
        reloadTheme = (pkgs.writeScriptBin "reloadTheme" ''
          #!${pkgs.stdenv.shell}
          echo "Reloading current theme: ${cfg.active}"
          ${concatStringsSep "\n" (mapAttrsToList (name: script: ''
            echo "[${name}]"
            ${script}
          '') cfg.onReload)}
        '');
      in {
        user.packages = [ reloadTheme ];
        system.userActivationScripts.reloadTheme = ''
          [ -z "$NORELOAD" ] && ${reloadTheme}/bin/reloadTheme
        '';
      }))
    ]))
  ]);
}
