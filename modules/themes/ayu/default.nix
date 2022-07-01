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
  deskCfg = config.modules.desktop;
in {
  config = mkIf (cfg.active == "ayu") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./wallpaper.jpg;

        gtk = {
          theme = "Orchis-Dark-Compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
          emoji = "Twitter Color Emoji";
        };

        colors = {
          black = "#1d242c";
          red = "#ff7733";
          green = "#b8cc52";
          yellow = "#ffb454";
          blue = "#36a3d9";
          magenta = "#ca30c7";
          cyan = "#95e6cb";
          white = "#c7c7c7";

          brightBlack = "#686868";
          brightRed = "#f07178";
          brightGreen = "#cbe645";
          brightYellow = "#ffee99";
          brightBlue = "#6871ff";
          brightMagenta = "#ff77ff";
          brightCyan = "#a6fde1";
          brightWhite = "#ffffff";

          types = {
            fg = "#e6e1cf";
            bg = "#0f1419";
            panelbg = "#171b24";
            border = "#f29718";
            highlight = "#e6e1cf";
          };
        };

        neovim.theme = "themer_ayu";

        vscode = {
          extension = {
            name = "ayu";
            publisher = "teabyii";
            version = "1.0.5";
            sha256 = "+ifqgwlikr+qjblmqlzf44xnbn7br5a119v9wanzou4=";
          };

          theme = {
            dark = "Ayu Dark";
            light = "Ayu Light";
          };
        };

        fish.colors = {
          fg = "#b3b1ad";
          highlight = "#e6b450";

          base01 = "#00827f";
          base02 = "#95e6cb";
          base03 = "#";
          base04 = "#e53983";
          base05 = "#ff3333";
          base06 = "#f29668";
          base07 = "#c2d94c";
          base08 = "#ffb454";
          base09 = "#59c2ff";
          base10 = "#4d5566";
        };

        rofi.colors = {
          bg = {
            main = "hsla(210, 25%, 7%, 1)";
            alt = "hsla(210, 25%, 7%, 0)";
            bar = "hsla(212, 20%, 14%, 1)";
          };
          fg = "hsla(46, 31%, 85%, 1)";

          ribbon = {
            outer = "hsla(211, 46%, 27%, 1)";
            inner = "hsla(197, 78%, 40%, 1)";
          };

          highlight = "hsla(209, 82%, 33%, 0.6)";
          urgent = "hsla(40, 100%, 50%, 1)";
          transparent = "hsla(0, 0%, 0%, 0)";
        };
      };

      # modules.desktop.browsers = {
      #   firefox.userChrome = concatMapStringsSep "\n" readFile
      #     [ ./config/firefox/userChrome.css ];
      # };
    }

    # Desktop (X11) theming <- Change after gnome = independent of xserver.
    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [
        orchis-theme
        whitesur-icon-theme
        bibata-cursors
      ];

      fonts.fonts = with pkgs; [
        (nerdfonts.override {fonts = ["VictorMono"];})
        twitter-color-emoji
      ];
    })

    (mkIf (deskCfg.xmonad.enable || deskCfg.qtile.enable) {
      services.xserver.displayManager = {
        sessionCommands = with cfg.gtk; ''
          ${getExe pkgs.xorg.xsetroot} -xcf ${pkgs.bibata-cursors}/share/icons/${cursor.name}/cursors/${cursor.default} ${
            toString (cursor.size)
          }
        '';

        # LightDM: Replace with LightDM-Web-Greeter theme
        lightdm.greeters.mini.extraConfig = ''
          text-color = "${cfg.colors.magenta}"
          password-background-color = "${cfg.colors.black}"
          window-color = "${cfg.colors.types.border}"
          border-color = "${cfg.colors.types.border}"
        '';
      };

      # TODO: Fcitx5 ayu-dark theme
      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "icy-thought";
        repo = "fcitx5-catppuccin";
        rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
        sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
      };
    })
  ]);
}
