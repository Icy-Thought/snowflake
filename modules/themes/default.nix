{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) getEnv map;
  inherit (lib.attrsets) attrValues mapAttrsToList;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) concatStringsSep optionalString;

  cfg = config.modules.themes;
  desktop = config.modules.desktop;
in {
  options.modules.themes = let
    inherit (lib.options) mkOption mkPackageOption;
    inherit (lib.types) attrsOf int lines listOf nullOr path package str;
    inherit (lib.my) mkOpt toFilteredImage;
  in {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v:
        let theme = getEnv "THEME";
        in if theme != "" then theme else v;
      description = ''
        Name of the theme which ought to be applied.
        Can be overridden by the `THEME` environment variable.
      '';
    };

    wallpaper = mkOpt (nullOr path) null;

    loginWallpaper = mkOpt (nullOr path) (if cfg.wallpaper != null then
      toFilteredImage cfg.wallpaper "-gaussian-blur 0x2 -modulate 70 -level 5%"
    else
      null);

    gtk = {
      name = mkOpt str "";
      package = mkPackageOption pkgs "gtk" { };
    };

    iconTheme = {
      name = mkOpt str "";
      package = mkPackageOption pkgs "icon" { };
    };

    pointer = {
      name = mkOpt str "";
      package = mkPackageOption pkgs "pointer" { };
      size = mkOpt int 24;
    };

    onReload = mkOpt (attrsOf lines) { };

    fontConfig = {
      packages = mkOpt (listOf package) [ ];
      mono = mkOpt (listOf str) [ "" ];
      sans = mkOpt (listOf str) [ "" ];
      emoji = mkOpt (listOf str) [ "" ];
    };

    font = {
      mono = {
        family = mkOpt str "";
        weight = mkOpt str "Bold";
        weightAlt = mkOpt str "Bold";
        weightNum = mkOpt int 700;
        size = mkOpt int 13;
      };
      sans = {
        family = mkOpt str "";
        weight = mkOpt str "SemiBold";
        weightAlt = mkOpt str "DemiBold";
        weightNum = mkOpt int 600;
        size = mkOpt int 10;
      };
    };

    colors = {
      main = {
        normal = {
          black = mkOpt str "#000000"; # 0
          red = mkOpt str "#FF0000"; # 1
          green = mkOpt str "#00FF00"; # 2
          yellow = mkOpt str "#FFFF00"; # 3
          blue = mkOpt str "#0000FF"; # 4
          magenta = mkOpt str "#FF00FF"; # 5
          cyan = mkOpt str "#00FFFF"; # 6
          white = mkOpt str "#BBBBBB"; # 7
        };
        bright = {
          black = mkOpt str "#888888"; # 8
          red = mkOpt str "#FF8800"; # 9
          green = mkOpt str "#00FF80"; # 10
          yellow = mkOpt str "#FF8800"; # 11
          blue = mkOpt str "#0088FF"; # 12
          magenta = mkOpt str "#FF88FF"; # 13
          cyan = mkOpt str "#88FFFF"; # 14
          white = mkOpt str "#FFFFFF"; # 15
        };
        types = let
          inherit (cfg.colors.main.normal) black red white yellow;
          inherit (cfg.colors.main.types) bg fg;
        in {
          bg = mkOpt str black;
          fg = mkOpt str white;
          panelbg = mkOpt str bg;
          panelfg = mkOpt str fg;
          border = mkOpt str bg;
          error = mkOpt str red;
          warning = mkOpt str yellow;
          highlight = mkOpt str white;
        };
      };

      rofi = {
        bg = {
          main = mkOpt str "#FFFFFF";
          alt = mkOpt str "#FFFFFF";
          bar = mkOpt str "#FFFFFF";
        };
        fg = mkOpt str "#FFFFFF";
        ribbon = {
          outer = mkOpt str "#FFFFFF";
          inner = mkOpt str "#FFFFFF";
        };
        selected = mkOpt str "#FFFFFF";
        urgent = mkOpt str "#FFFFFF";
        transparent = mkOpt str "#FFFFFF";
      };
    };

    editor = {
      helix = {
        light = mkOpt str "";
        dark = mkOpt str "";
      };
      neovim = {
        light = mkOpt str "";
        dark = mkOpt str "";
      };
      vscode = {
        dark = mkOpt str "";
        light = mkOpt str "";
        extension = {
          name = mkOpt str "";
          publisher = mkOpt str "";
          version = mkOpt str "";
          hash = mkOpt str "";
        };
      };
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    {
      # Allow HM to control GTK Theme:
      programs.dconf.enable = true;

      hm.gtk = let
        inherit (cfg.font) sans;
        inherit (cfg) gtk iconTheme;
      in {
        enable = true;
        font = {
          name = sans.family;
          size = sans.size;
        };
        theme = {
          name = gtk.name;
          package = gtk.package;
        };
        iconTheme = {
          name = iconTheme.name;
          package = iconTheme.package;
        };
        gtk3.bookmarks = map (dir: "file://${config.user.home}/" + dir) [
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

      home.pointerCursor = let inherit (cfg.pointer) name package size;
      in {
        name = name;
        package = package;
        size = size;
        gtk.enable = true;
      };

      fonts = let inherit (cfg.fontConfig) packages emoji mono sans;
      in {
        packages = packages;
        fontconfig.defaultFonts = {
          monospace = mono;
          sansSerif = sans;
          emoji = emoji;
        };
      };

      create.dataFile =
        mkIf (cfg.wallpaper != null) { "wallpaper".source = cfg.wallpaper; };

      hm.programs.vscode.extensions =
        let inherit (cfg.vscode.extension) name publisher version hash;
        in pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
          name = "${name}";
          publisher = "${publisher}";
          version = "${version}";
          hash = "${hash}";
        }];
    }

    (mkIf (desktop.type == "wayland") (mkMerge [
      {
        programs.regreet = let inherit (cfg) pointer font iconTheme gtk;
        in {
          theme = {
            name = "${gtk.name}";
            package = "${gtk.package}";
          };
          iconTheme = {
            name = "${iconTheme.name}";
            package = "${iconTheme.package}";
          };
          cursorTheme = {
            name = "${pointer.name}";
            package = "${pointer.package}";
          };
          font = {
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
          properties = let inherit (cfg.colors.main) bright normal types;
          in {
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
          greeters.mini.extraConfig =
            let inherit (cfg.colors.main) normal types;
            in ''
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
        reloadTheme = let inherit (pkgs) stdenv writeScriptBin;
        in (writeScriptBin "reloadTheme" ''
          #!${stdenv.shell}
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
