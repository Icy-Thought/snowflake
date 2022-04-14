{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./config/wallpaper.jpg;

        gtk = {
          # theme = "Catppuccin";
          theme = "Orchis-dark-compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        vscode.theme = {
          dark = "Catppuccin";
          light = "Quiet Light";
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
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
            panelbg = "#161320";
            border = "#abe9b3";
            highlight = "#575268";
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

      fonts.fonts = with pkgs;
        [ (nerdfonts.override { fonts = [ "VictorMono" ]; }) ];

      home.configFile = with config.modules;
        mkMerge [
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/90-theme".text = import ./config/Xresources cfg;
            "fish/conf.d/catppuccin.fish".source =
              ./config/fish/catppuccin.fish;
          }
          (mkIf (desktop.xmonad.enable || desktop.qtile.enable) {
            "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf desktop.terminal.alacritty.enable {
            "alacritty/config/catppuccin.yml".text =
              import ./config/alacritty/catppuccin.yml cfg;
          })
          (mkIf desktop.terminal.kitty.enable {
            "kitty/config/catppuccin.conf".text =
              import ./config/kitty/catppuccin.conf cfg;
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

    # Activate Neovim Colorscheme
    (mkIf config.modules.desktop.editors.nvim.enable {
      homeManager.programs.neovim.plugins = with pkgs.vimPlugins; [{
        plugin = catppuccin-nvim;
        type = "lua";
        config = builtins.readFile ./config/nvim/catppuccin.lua;
      }];
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
