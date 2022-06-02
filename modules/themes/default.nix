{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.themes;
in {
  options.modules.themes = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v: let
        theme = builtins.getEnv "THEME";
      in
        if theme != ""
        then theme
        else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable.
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
      cursor = {
        default = mkOpt str "left_ptr";
        name = mkOpt str "";
        size = mkOpt int "";
      };
    };

    neovim.theme = mkOpt str "";

    vscode.theme = {
      dark = mkOpt str "";
      light = mkOpt str "";
    };

    onReload = mkOpt (attrsOf lines) {};

    font = {
      mono = {
        family = mkOpt str "";
        weight = mkOpt str "SemiBold";
        weightNum = mkOpt str "600";
        size = mkOpt int 13;
      };
      sans = {
        family = mkOpt str "";
        weight = mkOpt str "SemiBold";
        weightNum = mkOpt str "600";
        size = mkOpt int 10;
      };
      emoji = mkOpt str "";
    };

    colors = {
      black = mkOpt str "#000000"; # 0
      red = mkOpt str "#FF0000"; # 1
      green = mkOpt str "#00FF00"; # 2
      yellow = mkOpt str "#FFFF00"; # 3
      blue = mkOpt str "#0000FF"; # 4
      magenta = mkOpt str "#FF00FF"; # 5
      cyan = mkOpt str "#00FFFF"; # 6
      white = mkOpt str "#BBBBBB"; # 7

      brightBlack = mkOpt str "#888888"; # 8
      brightRed = mkOpt str "#FF8800"; # 9
      brightGreen = mkOpt str "#00FF80"; # 10
      brightYellow = mkOpt str "#FF8800"; # 11
      brightBlue = mkOpt str "#0088FF"; # 12
      brightMagenta = mkOpt str "#FF88FF"; # 13
      brightCyan = mkOpt str "#88FFFF"; # 14
      brightWhite = mkOpt str "#FFFFFF"; # 15

      types = {
        bg = mkOpt str cfg.colors.black;
        fg = mkOpt str cfg.colors.white;
        panelbg = mkOpt str cfg.colors.types.bg;
        panelfg = mkOpt str cfg.colors.types.fg;
        border = mkOpt str cfg.colors.types.bg;
        error = mkOpt str cfg.colors.red;
        warning = mkOpt str cfg.colors.yellow;
        highlight = mkOpt str cfg.colors.white;
      };
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    # Read xresources files in ~/.config/xtheme/* to allow modular configuration
    # of Xresources.
    (let
      xrdb = ''cat "$XDG_CONFIG_HOME"/xtheme/* | ${getExe pkgs.xorg.xrdb} -load'';
    in {
      home.configFile."xtheme.init" = {
        text = xrdb;
        executable = true;
      };
      modules.themes.onReload.xtheme = xrdb;
    })

    {
      home.configFile = {
        "xtheme/00-init".text = with cfg.colors; ''
          #define bg   ${types.bg}
          #define fg   ${types.fg}
          #define blk  ${black}
          #define red  ${red}
          #define grn  ${green}
          #define ylw  ${yellow}
          #define blu  ${blue}
          #define mag  ${magenta}
          #define cyn  ${cyan}
          #define wht  ${white}
          #define bblk ${brightBlack}
          #define bred ${brightRed}
          #define bgrn ${brightGreen}
          #define bylw ${brightYellow}
          #define bblu ${brightBlue}
          #define bmag ${brightMagenta}
          #define bcyn ${brightCyan}
          #define bwht ${brightWhite}
        '';

        "xtheme/05-colors".text = ''
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

        "xtheme/05-fonts".text = with cfg.font.sans; ''
          *.font: xft:${family}:style=${weight}:pixelsize=${toString size}
          Emacs.font: ${family}:style=${weight}:pixelsize=${toString size}
        '';

        # GTK
        "gtk-3.0/settings.ini".text = ''
          [Settings]
          ${optionalString (cfg.gtk.theme != "")
            "gtk-theme-name=${cfg.gtk.theme}"}
          ${optionalString (cfg.gtk.iconTheme != "")
            "gtk-icon-theme-name=${cfg.gtk.iconTheme}"}
          ${optionalString (cfg.gtk.cursor.name != "")
            "gtk-cursor-theme-name=${cfg.gtk.cursor.name}"}
          ${optionalString (cfg.gtk.cursor.size != "")
            "gtk-cursor-theme-size=${toString (cfg.gtk.cursor.size)}"}
          gtk-fallback-icon-theme=gnome
          gtk-application-prefer-dark-theme=true
          gtk-xft-hinting=1
          gtk-xft-hintstyle=hintfull
          gtk-xft-rgba=none
        '';

        # GTK2 global theme (widget and icon theme)
        "gtk-2.0/gtkrc".text = ''
          ${optionalString (cfg.gtk.theme != "")
            ''gtk-theme-name="${cfg.gtk.theme}"''}
          ${optionalString (cfg.gtk.iconTheme != "")
            ''gtk-icon-theme-name="${cfg.gtk.iconTheme}"''}
          gtk-font-name="Sans ${toString (cfg.font.sans.size)}"
        '';

        # QT4/5 global theme
        "Trolltech.conf".text = ''
          [Qt]
          ${optionalString (cfg.gtk.theme != "") "style=${cfg.gtk.theme}"}
        '';
      };

      # Force Qt to use GTK cursor theme
      home.file.".icons/default/index.theme" = mkIf (cfg.gtk.cursor.name != "") {
        text = ''
          [icon theme]
          Inherits=${cfg.gtk.cursor.name}
        '';
      };

      fonts.fontconfig.defaultFonts = {
        sansSerif = [cfg.font.sans.family];
        monospace = [cfg.font.mono.family];
        emoji = [cfg.font.emoji];
      };
    }

    (mkIf (cfg.wallpaper != null)
      # Set the wallpaper ourselves so we don't need .background-image and/or
      # .fehbg polluting $HOME
      (let
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
        services.xserver.displayManager.sessionCommands = command;
        modules.themes.onReload.wallpaper = command;

        home.dataFile = mkIf (cfg.wallpaper != null) {
          "wallpaper".source = cfg.wallpaper;
        };
      }))

    (mkIf (cfg.loginWallpaper != null) {
      services.xserver.displayManager.lightdm.background = cfg.loginWallpaper;
    })

    (mkIf (cfg.onReload != {}) (let
      reloadTheme = with pkgs; (writeScriptBin "reloadTheme" ''
        #!${stdenv.shell}
        echo "Reloading current theme: ${cfg.active}"
        ${concatStringsSep "\n" (mapAttrsToList (name: script: ''
            echo "[${name}]"
            ${script}
          '')
          cfg.onReload)}
      '');
    in {
      user.packages = [reloadTheme];
      system.userActivationScripts.reloadTheme = ''
        [ -z "$NORELOAD" ] && ${reloadTheme}/bin/reloadTheme
      '';
    }))
  ]);
}
