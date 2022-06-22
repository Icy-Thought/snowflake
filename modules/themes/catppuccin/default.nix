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
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./config/wallpaper.png;

        gtk = {
          theme = "Orchis-Dark-Compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        neovim.theme = "catppuccin";

        vscode.theme = {
          dark = "Catppuccin";
          light = "Quiet Light";
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
          emoji = "Twitter Color Emoji";
        };

        colors = {
          black = "#6e6c7e";
          red = "#f28fad";
          green = "#abe9b3";
          yellow = "#fae3b0";
          blue = "#96cdfb";
          magenta = "#f5c2e7";
          cyan = "#89dceb";
          white = "#d9e0ee";

          brightBlack = "#988ba2";
          brightRed = "#f28fad";
          brightGreen = "#abe9b3";
          brightYellow = "#fae3b0";
          brightBlue = "#96cdfb";
          brightMagenta = "#f5c2e7";
          brightCyan = "#89dceb";
          brightWhite = "#d9e0ee";

          types = {
            fg = "#d9e0ee";
            bg = "#1e1d2f";
            panelbg = "#b5e8e0";
            border = "#c9cbff";
            highlight = "#f2cdcd";
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
            "fish/conf.d/catppuccin.fish".source =
              ./config/fish/catppuccin.fish;
          }
          (mkIf (xmonad.enable || qtile.enable) {
            "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf terminal.alacritty.enable {
            "alacritty/config/catppuccin.yml".text =
              import ./config/alacritty/catppuccin.yml cfg;
          })
          (mkIf terminal.kitty.enable {
            "kitty/config/catppuccin.conf".text =
              import ./config/kitty/catppuccin.conf cfg;
          })
          (mkIf terminal.wezterm.enable {
            "wezterm/config/catppuccin.lua".text =
              import ./config/wezterm/catppuccin.lua cfg;
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

      # Fcitx5
      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "icy-thought";
        repo = "fcitx5-catppuccin";
        rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
        sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
      };
    })
  ]);
}
