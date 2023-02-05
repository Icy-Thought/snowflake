{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) toString;
  inherit (lib) mkDefault mkIf mkMerge;

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

        editor = {
          helix = {
            dark = "ayu_dark";
            light = "ayu_light";
          };
          neovim = {
            dark = "ayu-dark";
            light = "ayu-light";
          };
          vscode = {
            dark = "Ayu Dark";
            light = "Ayu Light";
            extension = {
              name = "ayu";
              publisher = "teabyii";
              version = "1.0.5";
              hash = "sha256-+ifqgwlikr+qjblmqlzf44xnbn7br5a119v9wanzou4=";
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
      fonts.fonts = with pkgs; [ cfg.font.package twitter-color-emoji ];

      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${iconTheme.name}";
          font = with font;
            "${sans.family} ${sans.weight} ${toString (sans.size)}";
        };

        theme = let inherit (config.hm.lib.formats.rasi) mkLiteral;
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
          text-color = "${types.bg}"
          password-background-color = "${normal.black}"
          window-color = "${normal.yellow}"
          border-color = "${types.bg}"
        '';
      };
    })
  ]);
}
