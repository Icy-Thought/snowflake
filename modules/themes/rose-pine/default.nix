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
  config = mkIf (cfg.active == "rose-pine") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./config/wallpaper.png;

        gtk = {
          # theme = "Rosé Pine";
          theme = "rose-pine";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        neovim.theme = "rose-pine";

        vscode.theme = {
          dark = "Rosé Pine";
          light = "Rosé Pine Dawn";
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
        };

        colors = {
          black = "#26233a";
          red = "#eb6f92";
          green = "#31748f";
          yellow = "#f6c177";
          blue = "#9ccfd8";
          magenta = "#c4a7e7";
          cyan = "#ebbcba";
          white = "#e0def4";

          brightBlack = "#6e6a86";
          brightRed = "#eb6f92";
          brightGreen = "#31748f";
          brightYellow = "#f6c177";
          brightBlue = "#9ccfd8";
          brightMagenta = "#c4a7e7";
          brightCyan = "#ebbcba";
          brightWhite = "#e0def4";

          # REDO
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
        rose-pine-gtk-theme
        whitesur-icon-theme
        bibata-cursors
      ];

      fonts.fonts = with pkgs; [(nerdfonts.override {fonts = ["VictorMono"];})];

      home.configFile = with config.modules;
        mkMerge [
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/90-theme".text = import ./config/Xresources cfg;
            "fish/conf.d/rose-pine.fish".source = ./config/fish/rose-pine.fish;
          }
          (mkIf (desktop.xmonad.enable || desktop.qtile.enable) {
            "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf desktop.terminal.alacritty.enable {
            "alacritty/config/rose-pine.yml".text =
              import ./config/alacritty/rose-pine.yml cfg;
          })
          (mkIf desktop.terminal.kitty.enable {
            "kitty/config/rose-pine.conf".text =
              import ./config/kitty/rose-pine.conf cfg;
          })
          (mkIf desktop.media.docViewer.enable {
            "zathura/zathurarc".text = import ./config/zathura/zathurarc cfg;
          })
          # (mkIf desktop.media.graphics.vector.enable {
          #   "inkscape/templates/default.svg".source =
          #     ./config/inkscape/default-template.svg;
          # })
        ];
    })

    (mkIf (config.modules.desktop.xmonad.enable
      || config.modules.desktop.qtile.enable) {
      services.xserver.displayManager = {
        sessionCommands = with cfg.gtk; ''
          ${pkgs.xorg.xsetroot}/bin/xsetroot -xcf ${pkgs.bibata-cursors}/share/icons/${cursor.name}/cursors/${cursor.default} ${
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

      # Fcitx5: solution not available atm
      # home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
      #   owner = "icy-thought";
      #   repo = "fcitx5-rose-pine";
      #   rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
      #   sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
      # };
    })
  ]);
}
