{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "kanagawa") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./assets/antonio-brave-herakles.jpg;

        gtk = {
          name = "Orchis-Dark-Compact";
          package = pkgs.orchis-theme;
        };

        iconTheme = {
          name = "WhiteSur-dark";
          package = pkgs.whitesur-icon-theme;
        };

        pointer = {
          name = "Bibata-Modern-Classic";
          package = pkgs.bibata-cursors;
          size = 24;
        };

        font = {
          package = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
          emoji = "Twitter Color Emoji";
        };

        colors = {
          main = {
            normal = {
              black = "#090618";
              red = "#c34043";
              green = "#76946a";
              yellow = "#c0a36e";
              blue = "#7e9cd8";
              magenta = "#957fb8";
              cyan = "#6a9589";
              white = "#c8c093";
            };
            bright = {
              black = "#727169";
              red = "#e82424";
              green = "#98bb6c";
              yellow = "#e6c384";
              blue = "#7fb4ca";
              magenta = "#938aa9";
              cyan = "#7aa89f";
              white = "#dcd7ba";
            };
            types = {
              fg = "#dcd7ba";
              bg = "#1f1f28";
              panelbg = "#15161e";
              border = "#c8c093";
              highlight = "#2d4f67";
            };
          };

          fish = {
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

          rofi = {
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
            selected = "";
            urgent = "";
            transparent = "";
          };
        };

        editor = {
          helix = {
            dark = "kanagawa"; # FIXME: no helix theme as of 2022-09-01
            light = "kanagawa_light";
          };
          neovim = {
            dark = "kanagawa";
            light = "kanagawa"; # TODO: vim.g.background = "light"
          };
          vscode = {
            dark = "Kanagawa";
            light = "Quite Light";
            extension = {
              name = "kanagawa";
              publisher = "qufiwefefwoyn";
              version = "1.5.1";
              hash = "";
            };
          };
        };
      };
    }

    # (mkIf config.modules.desktop.browsers.firefox.enable {
    #   firefox.userChrome =
    #     concatMapStringsSep "\n" readFile
    #     ["${configDir}" /firefox/userChrome.css];
    # })

    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [ orchis-theme whitesur-icon-theme ];

      fonts.fonts = with pkgs; [
        cfg.font.package
        twitter-color-emoji
      ];

      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${iconTheme.name}";
          font = with font; "${sans.family} ${sans.weight} ${toString (sans.size)}";
        };

        theme =
          let inherit (config.hm.lib.formats.rasi) mkLiteral;
          in with cfg.colors.rofi; {
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

      hm.programs.sioyek.config = with cfg.font; {
        "custom_background_color " = "";
        "custom_text_color " = "";

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

    (mkIf (config.modules.desktop.envProto == "x11") {
      services.xserver.displayManager = {
        lightdm.greeters.mini.extraConfig = with cfg.colors.main; ''
          text-color = "${normal.magenta}"
          password-background-color = "${normal.black}"
          window-color = "${types.border}"
          border-color = "${types.border}"
        '';
      };
    })
  ]);
}
