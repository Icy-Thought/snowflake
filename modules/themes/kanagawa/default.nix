{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.themes;
  dsk = config.modules.desktop;
  dapl = config.modules.desktop.appliances;
in {
  config = mkIf (cfg.active == "kanagawa") (mkMerge [
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
            black = "#090618";
            red = "#c34043";
            green = "#76946a";
            yellow = "#c0a36e";
            blue = "#7e9cd8";
            magenta = "#957fb8";
            cyan = "#6a9589";
            white = "#c8c093";

            # Bright Colors
            brightBlack = "#727169";
            brightRed = "#e82424";
            brightGreen = "#98bb6c";
            brightYellow = "#e6c384";
            brightBlue = "#7fb4ca";
            brightMagenta = "#938aa9";
            brightCyan = "#7aa89f";
            brightWhite = "#dcd7ba";

            # Extra Colors:
            types = {
              fg = "#dcd7ba";
              bg = "#1f1f28";
              panelbg = "#15161e";
              border = "#c8c093";
              highlight = "#2d4f67";
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
          "fish/conf.d/kanagawa.fish".source = ./config/fish/kanagawa.fish;
        }
        (mkIf (dsk.xmonad.enable || dsk.qtile.enable) {
          "dunst/dunstrc".text = import ./config/dunst/dunstrc cfg;
          "rofi" = {
            source = ./config/rofi;
            recursive = true;
          };
        })
        (mkIf dapl.termEmu.alacritty.enable {
          "alacritty/config/kanagawa.yml".text =
            import ./config/alacritty/kanagawa.yml cfg;
        })
        (mkIf dapl.termEmu.kitty.enable {
          "kitty/config/kanagawa.conf".text =
            import ./config/kitty/kanagawa.conf cfg;
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
