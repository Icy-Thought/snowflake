{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) toString;
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkDefault mkIf mkMerge;

  cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "ayu") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./wallpaper.jpg; # FIXME

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
        };

        fontConfig = {
          packages = attrValues {
            inherit (pkgs) noto-fonts-emoji sarasa-gothic;
            google-fonts = pkgs.google-fonts.override { fonts = [ "Cardo" ]; };
            nerdfonts = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
          };
          mono = [ "VictorMono Nerd Font" "Sarasa Mono SC" ];
          sans = [ "Sarasa Gothic SC" ];
          emoji = [ "Noto Color Emoji" ];
        };

        font = {
          mono.family = "VictorMono Nerd Font";
          sans.family = "Sarasa Gothic SC";
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

        editor = {
          helix = {
            dark = "ayu_dark";
            light = "ayu_light";
          };
          neovim = {
            dark = "ayu-dark";
            light = "ayu-light";
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
      hm.programs.rofi = {
        extraConfig = {
          icon-theme = let inherit (cfg.iconTheme) name; in "${name}";
          font = let inherit (cfg.font.sans) family weight size;
          in "${family} ${weight} ${toString size}";
        };

        theme = let
          inherit (config.hm.lib.formats.rasi) mkLiteral;
          inherit (cfg.colors.rofi) bg fg ribbon selected transparent urgent;
        in {
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

      hm.programs.sioyek.config =
        let inherit (cfg.font.mono) family size weight;
        in {
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

          "font_size" = "${toString size}";
          "ui_font" = "${family} ${weight}";
        };
    })
  ]);
}
