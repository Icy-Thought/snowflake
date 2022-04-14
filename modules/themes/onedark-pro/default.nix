{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "onedark-pro") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./config/wallpaper.jpg;

        gtk = {
          theme = "Orchis-dark-compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Amber";
            size = 24;
          };
        };

        vscode.theme = {
          dark = "OneDarkPro";
          light = "Quite Light";
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
        };

        colors = {
          black = "#1e2127";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#abb2bf";

          brightBlack = "#5c6370";
          brightRed = "#e06c75";
          brightGreen = "#98c379";
          brightYellow = "#d19a66";
          brightBlue = "#61afef";
          brightMagenta = "#c678dd";
          brightCyan = "#56b6c2";
          brightWhite = "#ffffff";

          types = {
            fg = "#abb2bf";
            bg = "#1e2127";
            panelbg = "#1e2127";
            border = "#5c6370"; # TODO: change later..
            highlight = "#3e4451";
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
            "fish/conf.d/onedark-pro.fish".source =
              ./config/fish/onedark-pro.fish;
          }
          (mkIf (desktop.xmonad.enable || desktop.bspwm.enable) {
            "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf desktop.terminal.alacritty.enable {
            "alacritty/config/onedark-pro.yml".text =
              import ./config/alacritty/onedark-pro.yml cfg;
          })
          (mkIf desktop.terminal.kitty.enable {
            "kitty/config/onedark-pro.conf".text =
              import ./config/kitty/onedark-pro.conf cfg;
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
        plugin = onedarkpro-nvim;
        type = "lua";
        config = builtins.readFile ./config/nvim/onedark-pro.lua;
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
