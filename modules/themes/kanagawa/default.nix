{ options, config, lib, pkgs, ... }:
let
  inherit (builtins) toString readFile;
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkDefault mkIf mkMerge;
  inherit (lib.strings) concatMapStringsSep;

  cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "kanagawa") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./assets/morganprost-spacestation01.jpg;
        gtk = {
          name = "Kanagawa-BL";
          package = pkgs.my.kanagawa-gtk.override { themeVariants = [ "BL" ]; };
        };
        iconTheme = {
          name = "Kanagawa";
          package = pkgs.my.kanagawa-gtk.override { themeVariants = [ "BL" ]; };
        };
        pointer = {
          name = "Bibata-Modern-Classic";
          package = pkgs.bibata-cursors;
        };
        fontConfig = {
          packages = attrValues {
            inherit (pkgs) noto-fonts-emoji sarasa-gothic;
            google-fonts = pkgs.google-fonts.override { fonts = [ "Cardo" ]; };
            nerdfonts = pkgs.nerdfonts.override {
              fonts = [ "CascadiaCode" "VictorMono" ];
            };
          };
          mono = [ "VictorMono Nerd Font" "Sarasa Mono SC" ];
          sans = [ "Caskaydia Cove Nerd Font" "Sarasa Gothic SC" ];
          emoji = [ "Noto Color Emoji" ];
        };

        font = {
          mono.family = "VictorMono Nerd Font";
          sans.family = "CaskaydiaCove Nerd Font";
        };

        colors = {
          main = {
            normal = {
              black = "#090618";
              red = "#C34043";
              green = "#76946A";
              yellow = "#C0A36E";
              blue = "#7E9CD8";
              magenta = "#957FB8";
              cyan = "#6A9589";
              white = "#C8C093";
            };
            bright = {
              black = "#727169";
              red = "#FF5D62";
              green = "#98BB6C";
              yellow = "#E6C384";
              blue = "#7FB4CA";
              magenta = "#938AA9";
              cyan = "#7AA89F";
              white = "#DCD7BA";
            };
            types = {
              fg = "#DCD7BA";
              bg = "#1F1F28";
              panelbg = "#72A7BC";
              border = "#76946A";
              highlight = "#957FB8";
            };
          };

          rofi = {
            bg = {
              main = "hsla(240, 13%, 14%, 1)";
              alt = "hsla(240, 13%, 19%, 0)";
              bar = "hsla(240, 13%, 24%, 1)";
            };
            fg = "hsla(51, 33%, 80%, 1)";
            ribbon = {
              outer = "hsla(33, 64%, 62%, 1)";
              inner = "hsla(103, 17%, 50%, 1)";
            };
            selected = "hsla(199, 19%, 49%, 1)";
            urgent = "hsl(359, 52%, 51%, 1)";
            transparent = "hsla(0, 0%, 0%, 0)";
          };
        };

        editor = {
          helix = {
            dark = "kanagawa";
            light = "kanagawa";
          };
          neovim = {
            dark = "kanagawa";
            light = "Kanagawa_Light"; # :TODO| Integrate with Neovim
          };
          vscode = {
            dark = "Kanagawa";
            light = "Kanagawa Light";
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

    (mkIf config.modules.desktop.browsers.firefox.enable {
      modules.desktop.browsers.firefox.userChrome =
        let usrChromeDir = "${config.snowflake.configDir}/firefox/userChrome";
        in concatMapStringsSep "\n" readFile [ "${usrChromeDir}/sidebery.css" ];
    })

    (mkIf config.services.xserver.enable {
      hm.programs.rofi = {
        extraConfig = {
          icon-theme = let inherit (cfg.iconTheme) name; in "${name}";
          font = let inherit (cfg.font) mono sans;
          in "${mono.family} Italic ${mono.weight} ${toString sans.size}";
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
            text-color = mkLiteral "@fg";
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

      hm.programs.sioyek.config = let inherit (cfg.font) mono sans;
      in {
        "custom_background_color " = "0.10 0.11 0.15";
        "custom_text_color " = "0.75 0.79 0.96";

        "text_highlight_color" = "0.24 0.35 0.63";
        "visual_mark_color" = "1.00 0.62 0.39 1.0";
        "search_highlight_color" = "0.97 0.46 0.56";
        "link_highlight_color" = "0.48 0.64 0.97";
        "synctex_highlight_color" = "0.62 0.81 0.42";

        "page_separator_width" = "2";
        "page_separator_color" = "0.81 0.79 0.76";
        "status_bar_color" = "0.34 0.37 0.54";

        "font_size" = "${toString sans.size}";
        "ui_font" = "${mono.family} ${mono.weight}";
      };
    })
  ]);
}
