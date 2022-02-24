{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.themes;
  dsk = config.modules.desktop;
  dapl = config.modules.desktop.appliances;
in {
  config = mkIf (cfg.active == "ayu-dark") (mkMerge [
    {
      modules = {
        themes = {
          wallpaper = mkDefault ./config/wallpaper.jpg;
          gtk = {
            theme = "Orchis-dark-compact";
            iconTheme = "WhiteSur-dark";
            cursor = {
              name = "Bibata-Modern-Amber";
              size = 24;
            };
          };
          font = {
            sans.family = "VictorMono Nerd Font";
            mono.family = "VictorMono Nerd Font Mono";
          };
          colors = {
            # Normal Colors:
            black = "#1d242c";
            red = "#ff7733";
            green = "#b8cc52";
            yellow = "#ffb454";
            blue = "#36a3d9";
            magenta = "#ca30c7";
            cyan = "#95e6cb";
            white = "#c7c7c7";

            # Bright Colors
            brightBlack = "#686868";
            brightRed = "#f07178";
            brightGreen = "#cbe645";
            brightYellow = "#ffee99";
            brightBlue = "#6871ff";
            brightMagenta = "#ff77ff";
            brightCyan = "#a6fde1";
            brightWhite = "#ffffff";

            # Extra Colors:
            types = {
              fg = "#e6e1cf";
              bg = "#0f1419";
              panelbg = "#171b24";
              border = "#f29718";
              highlight = "#e6e1cf";
            };
          };
        };

        # desktop.browsers = {
        #   firefox.userChrome = concatMapStringsSep "\n" readFile
        #     [ ./config/firefox/userChrome.css ];
        # };
      };
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

      home.configFile = mkMerge [
        {
          # Sourced from sessionCommands in modules/themes/default.nix
          "xtheme/90-theme".text = import ./config/Xresources cfg;
          "fish/conf.d/ayu-dark.fish".source = ./config/fish/ayu-dark.fish;
        }
        (mkIf (dsk.xmonad.enable || dsk.qtile.enable) {
          "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
          "rofi" = {
            source = ./config/rofi;
            recursive = true;
          };
        })
        (mkIf dapl.termEmu.alacritty.enable {
          "alacritty/config/ayu-dark.yml".text =
            import ./config/alacritty/ayu-dark.yml cfg;
        })
        (mkIf dapl.termEmu.kitty.enable {
          "kitty/config/ayu-dark.conf".text =
            import ./config/kitty/ayu-dark.conf cfg;
        })
        (mkIf dapl.media.docViewer.enable {
          "zathura/zathurarc".text = import ./config/zathura/zathurarc cfg;
        })
        # (mkIf dapl.media.graphics.vector.enable {
        #   "inkscape/templates/default.svg".source =
        #     ./config/inkscape/default-template.svg;
        # })
      ];
    })

    (mkIf (dsk.xmonad.enable || dsk.qtile.enable) {
      services.xserver.displayManager = {
        sessionCommands = with cfg.gtk; ''
          ${pkgs.xorg.xsetroot}/bin/xsetroot -xcf ${pkgs.bibata-cursors}/share/icons/${cursor.name}/cursors/${cursor.default} ${
            toString (cursor.size)
          }
        '';

        sddm.theme = "${(pkgs.fetchFromGitHub {
          owner = "3ximus";
          repo = "aerial-sddm-theme";
          rev = "2fa0a4024bab60b0ba40de274880e0c1aa6eca59";
          sha256 = "jaGQaClD7Hk4eWh+rMX8ZtcGDzb9aCu+NX5gzJ1JXQg=";
        })}";
      };

      environment.systemPackages = with pkgs; [
        qt5.qtmultimedia
        libsForQt5.qt5.qtgraphicaleffects
      ];

      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "icy-thought";
        repo = "fcitx5-catppuccin";
        rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
        sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
      };
    })
  ]);
}
