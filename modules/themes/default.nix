{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let
  cfg = config.modules.themes;
  dsktp = config.modules.desktop;
in
{
  options.modules.themes = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v:
        let theme = builtins.getEnv "THEME";
        in if theme != ""
        then theme
        else v;
      description = ''
        Name of the theme which ought to be applied. 
        Can be overridden by the `THEME` environment variable.
      '';
    };

    wallpaper = mkOpt (either path null) null;

    loginWallpaper = mkOpt (either path null) (
      if cfg.wallpaper != null
      then toFilteredImage cfg.wallpaper "-gaussian-blur 0x2 -modulate 70 -level 5%"
      else null
    );

    gtk = {
      theme = mkOpt str "";
      iconTheme = mkOpt str "";
    };

    pointer = {
      name = mkOpt str "";
      package = mkOpt (either package null) null;
      size = mkOpt int "";
    };

    onReload = mkOpt (attrsOf lines) { };

    font = {
      mono = {
        family = mkOpt str "";
        weight = mkOpt str "SemiBold";
        weightAlt = mkOpt str "DemiBold";
        weightNum = mkOpt str "600";
        size = mkOpt int 13;
      };
      sans = {
        family = mkOpt str "";
        weight = mkOpt str "SemiBold";
        weightAlt = mkOpt str "DemiBold";
        weightNum = mkOpt str "600";
        size = mkOpt int 10;
      };
      emoji = mkOpt str "";
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
        types = with cfg.colors.main; {
          bg = mkOpt str normal.black;
          fg = mkOpt str normal.white;
          panelbg = mkOpt str types.bg;
          panelfg = mkOpt str types.fg;
          border = mkOpt str types.bg;
          error = mkOpt str normal.red;
          warning = mkOpt str normal.yellow;
          highlight = mkOpt str normal.white;
        };
      };

      fish = {
        fg = mkOpt str "#ffffff";
        highlight = mkOpt str "#ffffff";
        base01 = mkOpt str "#ffffff";
        base02 = mkOpt str "#ffffff";
        base03 = mkOpt str "#ffffff";
        base04 = mkOpt str "#ffffff";
        base05 = mkOpt str "#ffffff";
        base06 = mkOpt str "#ffffff";
        base07 = mkOpt str "#ffffff";
        base08 = mkOpt str "#ffffff";
        base09 = mkOpt str "#ffffff";
        base10 = mkOpt str "#ffffff";
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
      home.configFile = {
        gtk3-settings = {
          target = "gtk-3.0/settings.ini";
          text = with cfg.gtk; ''
            [Settings]
            ${optionalString (theme != "") "gtk-theme-name=${theme}"}

            ${optionalString (iconTheme != "")
              "gtk-icon-theme-name=${iconTheme}"}

            gtk-fallback-icon-theme=gnome
            gtk-application-prefer-dark-theme=true
            gtk-xft-hinting=1
            gtk-xft-hintstyle=hintfull
            gtk-xft-rgba=none
          '';
        };
        gtk2-settings = {
          target = "gtk-2.0/gtkrc";
          text = with cfg; ''
            ${optionalString (gtk.theme != "")
              ''gtk-theme-name="${gtk.theme}"''}
            ${optionalString (gtk.iconTheme != "")
              ''gtk-icon-theme-name="${gtk.iconTheme}"''}
            gtk-font-name="Sans ${toString (font.sans.size)}"
          '';
        };
        qt-settings = {
          target = "Trolltech.conf";
          text = with cfg.gtk; ''
            [Qt]
            ${optionalString (theme != "") "style=${theme}"}
          '';
        };
      };

      home.pointerCursor = with cfg.pointer; {
        name = name;
        package = package;
        size = size;
        gtk.enable = true;
      };

      fonts.fontconfig.defaultFonts = with cfg.font; {
        sansSerif = [ sans.family ];
        monospace = [ mono.family ];
        emoji = [ emoji ];
      };
    }

    # x11 related settings:
    (mkIf (dsktp.envProto == "x11") (
      let xrdb = ''cat "$XDG_CONFIG_HOME"/xtheme/* | ${getExe pkgs.xorg.xrdb} -load'';
      in {
        # Read xresources files in ~/.config/xtheme/* to allow modular configuration
        # of Xresources.
        modules.themes.onReload.xtheme = xrdb;

        home.configFile = {
          xtheme-init = {
            target = "xtheme.init";
            text = xrdb;
            executable = true;
          };

          xtheme-definitions = {
            target = "xtheme/00-init";
            text = with cfg.colors.main; ''
              #define bg   ${types.bg}
              #define fg   ${types.fg}
              #define blk  ${normal.black}
              #define bblk ${bright.black}
              #define red  ${normal.red}
              #define bred ${bright.red}
              #define grn  ${normal.green}
              #define bgrn ${bright.green}
              #define ylw  ${normal.yellow}
              #define bylw ${bright.yellow}
              #define blu  ${normal.blue}
              #define bblu ${bright.blue}
              #define mag  ${normal.magenta}
              #define bmag ${bright.magenta}
              #define cyn  ${normal.cyan}
              #define bcyn ${bright.cyan}
              #define wht  ${normal.white}
              #define bwht ${bright.white}
            '';
          };

          xtheme-colors = {
            target = "xtheme/05-colors";
            text = ''
              *.foreground: fg
              *.background: bg
              *.color0:  blk
              *.color1:  red
              *.color2:  grn
              *.color3:  ylw
              *.color4:  blu
              *.color5:  mag
              *.color6:  cyn
              *.color7:  wht
              *.color8:  bblk
              *.color9:  bred
              *.color10: bgrn
              *.color11: bylw
              *.color12: bblu
              *.color13: bmag
              *.color14: bcyn
              *.color15: bwht
            '';
          };

          xtheme-fonts = {
            target = "xtheme/05-fonts";
            text = with cfg.font.mono; ''
              *.font: xft:${family}:style=${weight}:pixelsize=${toString size}
              Emacs.font: ${family}:style=${weight}:pixelsize=${toString size}
            '';
          };

          xtheme-cursor = {
            target = "xtheme/06-cursor";
            text = with cfg.pointer; ''
              Xcursor.name: left_ptr
              Xcursor.size: ${toString size}
              Xcursor.theme: ${name}
            '';
          };
        };

        home.pointerCursor.x11 = {
          enable = true;
          defaultCursor = "left_ptr";
        };
      }
    ))

    # Set the wallpaper ourselves so we don't need .background-image and/or
    # .fehbg polluting $HOME
    (mkIf (cfg.wallpaper != null) && (dsktp.envProto == "x11") (
      let
        wCfg = config.services.xserver.desktopManager.wallpaper;
        command = ''
          if [ -e "$XDG_DATA_HOME/wallpaper" ]; then
            ${getExe pkgs.feh} --bg-${wCfg.mode} \
            ${optionalString wCfg.combineScreens "--no-xinerama"} \
            --no-fehbg \
            $XDG_DATA_HOME/wallpaper
          fi
        '';
      in
      {
        services.xserver.displayManager.sessionCommands = command;
        modules.themes.onReload.wallpaper = command;

        home.dataFile = mkIf (cfg.wallpaper != null) {
          "wallpaper".source = cfg.wallpaper;
        };
      }
    ))

    (mkIf (cfg.loginWallpaper != null) {
      services.xserver.displayManager = {
        lightdm.background = cfg.loginWallpaper;
      };
    })

    (mkIf (cfg.onReload != { }) (
      let
        reloadTheme = with pkgs; (writeScriptBin "reloadTheme" ''
          #!${stdenv.shell}
          echo "Reloading current theme: ${cfg.active}"
          ${concatStringsSep "\n" (mapAttrsToList (name: script: ''
              echo "[${name}]"
              ${script}
            '')
            cfg.onReload)}
        '');
      in
      {
        user.packages = [ reloadTheme ];
        system.userActivationScripts.reloadTheme = ''
          [ -z "$NORELOAD" ] && ${reloadTheme}/bin/reloadTheme
        '';
      }
    ))
  ]);
}
