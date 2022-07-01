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
  config = mkIf (cfg.active == "decay") (mkMerge [
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

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
          emoji = "Twitter Color Emoji";
        };

        colors = {
          black = "#242931";
          red = "#e05f65";
          green = "#78dba9";
          yellow = "#f1cf8a";
          blue = "#70a5eb";
          magenta = "#c68aee";
          cyan = "#74bee9";
          white = "#dee1e6";

          brightBlack = "#485263";
          brightRed = "#e05f65";
          brightGreen = "#78dba9";
          brightYellow = "#f1cf8a";
          brightBlue = "#70a5eb";
          brightMagenta = "#c68aee";
          brightCyan = "#74bee9";
          brightWhite = "#dee1e6";

          types = {
            fg = "#b6beca";
            bg = "#171b20";
            panelbg = "#ffeba6";
            border = "#8cc1ff";
            highlight = "#94f7c5";
          };
        };

        neovim.theme = "catppuccin";

        vscode = {
          extension = {
            name = "";
            publisher = "";
            version = "";
            sha256 = "";
          };

          theme = {
            dark = "decay"; #TODO: fix with proper naming
            light = "Quite Light";
          };
        };

        fish.colors = {
          fg = "#";
          highlight = "#";

          base01 = "#";
          base02 = "#";
          base03 = "#";
          base04 = "#";
          base05 = "#";
          base06 = "#";
          base07 = "#";
          base08 = "#";
          base09 = "#";
          base10 = "#";
        };

        rofi.colors = {
          bg = {
            main = "";
            alt = "";
            bar = "";
          };
          fg = "";

          ribbon = {
            outer = "";
            inner = "";
          };

          highlight = "";
          urgent = "";
          transparent = "";
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
