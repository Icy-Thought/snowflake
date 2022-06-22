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
        wallpaper = mkDefault ./config/wallpaper.jpg;

        gtk = {
          theme = "Orchis-Dark-Compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        neovim.theme = "themer_ayu";

        vscode.theme = {
          dark = "Ayu Dark";
          light = "Ayu Light";
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

      home.configFile = with deskCfg;
        mkMerge [
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/90-theme".text = import ./config/Xresources cfg;
            "fish/conf.d/ayu-dark.fish".source = ./config/fish/ayu-dark.fish;
          }
          (mkIf (xmonad.enable || qtile.enable) {
            "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf terminal.alacritty.enable {
            "alacritty/config/ayu-dark.yml".text =
              import ./config/alacritty/ayu-dark.yml cfg;
          })
          (mkIf terminal.kitty.enable {
            "kitty/config/ayu-dark.conf".text =
              import ./config/kitty/ayu-dark.conf cfg;
          })
          (mkIf terminal.wezterm.enable {
            "wezterm/config/ayu-dark.lua".text =
              import ./config/wezterm/ayu-dark.lua cfg;
          })
          (mkIf media.viewer.document.enable {
            "zathura/zathurarc".text = import ./config/zathura/zathurarc cfg;
          })
          # (mkIf media.editor.vector.enable {
          #   "inkscape/templates/default.svg".source =
          #     ./config/inkscape/default-template.svg;
          # })
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
