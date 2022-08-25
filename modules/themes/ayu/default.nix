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
  configDir = config.snowflake.configDir;
in {
  config = mkIf (cfg.active == "ayu") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./wallpaper.jpg;

        gtk = {
          theme = "Orchis-Dark-Compact";
          iconTheme = "WhiteSur-dark";
          cursor = {
            name = "Bibata-Modern-Classic";
            size = 24;
          };
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
          emoji = "Twitter Color Emoji";
        };

        colors = {
          main = {
            normal = {
              black = "#1d242c";
              red = "#ff7733";
              green = "#b8cc52";
              yellow = "#ffb454";
              blue = "#36a3d9";
              magenta = "#ca30c7";
              cyan = "#95e6cb";
              white = "#c7c7c7";
            };
            bright = {
              black = "#686868";
              red = "#f07178";
              green = "#cbe645";
              yellow = "#ffee99";
              blue = "#6871ff";
              magenta = "#ff77ff";
              cyan = "#a6fde1";
              white = "#ffffff";
            };
            types = {
              fg = "#e6e1cf";
              bg = "#0f1419";
              panelbg = "#171b24";
              border = "#f29718";
              highlight = "#e6e1cf";
            };
          };

          fish = {
            fg = "b3b1ad";
            highlight = "e6b450";
            base01 = "00827f";
            base02 = "95e6cb";
            base03 = "";
            base04 = "e53983";
            base05 = "ff3333";
            base06 = "f29668";
            base07 = "c2d94c";
            base08 = "ffb454";
            base09 = "59c2ff";
            base10 = "4d5566";
          };

          rofi = {
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
            selected = "hsla(209, 82%, 33%, 0.6)";
            urgent = "hsla(40, 100%, 50%, 1)";
            transparent = "hsla(0, 0%, 0%, 0)";
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
      };
    }

    # (mkIf config.modules.desktop.browsers.firefox.enable {
    #   firefox.userChrome =
    #     concatMapStringsSep "\n" readFile
    #     ["${configDir}" /firefox/userChrome.css];
    # })

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

    (mkIf (config.modules.desktop.xmonad.enable
      || config.modules.desktop.qtile.enable) {
      services.xserver.displayManager = {
        sessionCommands = with cfg.gtk; ''
          ${getExe pkgs.xorg.xsetroot} -xcf ${pkgs.bibata-cursors}/share/icons/${cursor.name}/cursors/${cursor.default} ${
            toString (cursor.size)
          }
        '';

        # LightDM: Replace with LightDM-Web-Greeter theme
        lightdm.greeters.mini.extraConfig = with cfg.colors.main; ''
          text-color = "${types.bg}"
          password-background-color = "${normal.black}"
          window-color = "${normal.yellow}"
          border-color = "${types.bg}"
        '';
      };
    })

    (mkIf config.modules.desktop.extra.fcitx5.enable {
      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "icy-thought";
        repo = "fcitx5-catppuccin";
        rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
        sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
      };
    })

    (mkIf config.modules.desktop.media.document.sioyek.enable {
      hm.programs.sioyek.config = with cfg.font; {
        "custom_background_color " = "";
        "custom_text_color " = "";
        "startup_commands" = "toggle_custom_color";

        "text_highlight_color" = "";
        "visual_mark_color" = "";
        "search_highlight_color" = "";
        "link_highlight_color" = "";
        "synctex_highlight_color" = "";

        "page_separator_width" = "2";
        "page_separator_color" = "";
        "status_bar_color" = "";

        "font_size" = "${toString (mono.size)}";
        "ui_font" = "${mono.family} ${mono.weight}";
      };
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with cfg.vscode.extension;
        pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "${name}";
            publisher = "${publisher}";
            version = "${version}";
            sha256 = "${sha256}";
          }
        ];
    })

    (mkIf config.modules.desktop.extra.rofi.enable {
      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${gtk.iconTheme}";
          font = with font; "${sans.family} ${sans.weight} ${toString (sans.size)}";
        };

        theme = let
          inherit (config.hm.lib.formats.rasi) mkLiteral;
        in
          with cfg.colors.rofi; {
            "*" = {
              fg = mkLiteral "${fg}";
              bg = mkLiteral "${bg.main}";
              bg-alt = mkLiteral "${bg.alt}";
              bg-bar = mkLiteral "${bg.bar}";

              outer-ribbon = mkLiteral "${ribbon.outer}";
              inner-ribbon = mkLiteral "${ribbon.inner}";
              selected = mkLiteral "${selected}";
              urgent = mkLiteral "${urgent}";
              transparent = mkLiteral "${transparent}";
            };

            "window" = {
              transparency = "real";
              background-color = mkLiteral "@bg";
              text-color = mkLiteral "@fg";
              border = mkLiteral "0% 0% 0% 1.5%";
              border-color = mkLiteral "@outer-ribbon";
              border-radius = mkLiteral "0% 0% 0% 2.5%";
              height = mkLiteral "54.50%";
              width = mkLiteral "43%";
              location = mkLiteral "center";
              x-offset = 0;
              y-offset = 0;
            };

            "prompt" = {
              enabled = true;
              padding = mkLiteral "0% 1% 0% 0%";
              background-color = mkLiteral "@bg-bar";
              text-color = mkLiteral "@fg";
            };

            "entry" = {
              background-color = mkLiteral "@bg-bar";
              text-color = mkLiteral "@fg";
              placeholder-color = mkLiteral "@fg";
              expand = true;
              horizontal-align = 0;
              placeholder = "Search";
              padding = mkLiteral "0.15% 0% 0% 0%";
              blink = true;
            };

            "inputbar" = {
              children = mkLiteral "[ prompt, entry ]";
              background-color = mkLiteral "@bg-bar";
              text-color = mkLiteral "@fg";
              expand = false;
              border = mkLiteral "0% 0% 0.3% 0.2%";
              border-radius = mkLiteral "1.5% 1.0% 1.5% 1.5%";
              border-color = mkLiteral "@inner-ribbon";
              margin = mkLiteral "0% 17% 0% 0%";
              padding = mkLiteral "1%";
              position = mkLiteral "center";
            };

            "listview" = {
              background-color = mkLiteral "@bg";
              columns = 5;
              spacing = mkLiteral "1%";
              cycle = false;
              dynamic = true;
              layout = mkLiteral "vertical";
            };

            "mainbox" = {
              background-color = mkLiteral "@bg";
              border = mkLiteral "0% 0% 0% 1.5%";
              border-radius = mkLiteral "0% 0% 0% 2.5%";
              border-color = mkLiteral "@inner-ribbon";
              children = mkLiteral "[ inputbar, listview ]";
              spacing = mkLiteral "3%";
              padding = mkLiteral "2.5% 2% 2.5% 2%";
            };

            "element" = {
              background-color = mkLiteral "@bg-bar";
              text-color = mkLiteral "@fg";
              orientation = mkLiteral "vertical";
              border-radius = mkLiteral "1.5% 1.0% 1.5% 1.5%";
              padding = mkLiteral "2% 0% 2% 0%";
            };

            "element-icon" = {
              background-color = mkLiteral "@transparent";
              text-color = mkLiteral "inherit";
              horizontal-align = "0.5";
              vertical-align = "0.5";
              size = mkLiteral "64px";
              border = mkLiteral "0px";
            };

            "element-text" = {
              background-color = mkLiteral "@transparent";
              text-color = mkLiteral "inherit";
              expand = true;
              horizontal-align = mkLiteral "0.5";
              vertical-align = mkLiteral "0.5";
              margin = mkLiteral "0.5% 1% 0% 1%";
            };

            "element normal.urgent, element alternate.urgent" = {
              background-color = mkLiteral "@urgent";
              text-color = mkLiteral "@fg";
              border-radius = mkLiteral "1%";
            };

            "element normal.active, element alternate.active" = {
              background-color = mkLiteral "@bg-alt";
              text-color = mkLiteral "@fg";
            };

            "element selected" = {
              background-color = mkLiteral "@selected";
              text-color = mkLiteral "@bg";
              border = mkLiteral "0% 0% 0.3% 0.2%";
              border-radius = mkLiteral "1.5% 1.0% 1.5% 1.5%";
              border-color = mkLiteral "@inner-ribbon";
            };

            "element selected.urgent" = {
              background-color = mkLiteral "@urgent";
              text-color = mkLiteral "@fg";
            };

            "element selected.active" = {
              background-color = mkLiteral "@bg-alt";
              color = mkLiteral "@fg";
            };
          };
      };
    })
  ]);
}
