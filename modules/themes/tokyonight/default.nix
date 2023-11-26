{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) toString readFile;
  inherit (lib.attrsets) attrValues;
  inherit (lib.attrsets) concatMapStringsSep;
  inherit (lib.modules) mkDefault mkIf mkMerge;

  cfg = config.modules.themes;
in {
  config = mkIf (cfg.active == "tokyonight") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./assets/avatar-H2O-upscayl.png;

        gtk = {
          name = "Tokyonight-Dark-BL";
          package =
            pkgs.my.tokyonight-gtk.override {themeVariants = ["Dark-BL"];};
        };

        iconTheme = {
          name = "Fluent-orange-dark";
          package =
            pkgs.fluent-icon-theme.override {colorVariants = ["orange"];};
        };

        pointer = {
          name = "Bibata-Modern-Classic";
          package = pkgs.bibata-cursors;
          size = 24;
        };

        fontConfig = {
          packages = attrValues {
            inherit (pkgs) dancing-script noto-fonts-emoji sarasa-gothic;
            nerdfonts = pkgs.nerdfonts.override {fonts = ["VictorMono"];};
          };
          mono = ["VictorMono Nerd Font Mono"];
          sans = ["VictorMono Nerd Font"];
          emoji = ["Noto Color Emoji"];
        };

        font = {
          sans.family = "VictorMono Nerd Font";
          mono.family = "VictorMono Nerd Font Mono";
        };

        colors = {
          main = {
            normal = {
              black = "#15161e";
              red = "#f7768e";
              green = "#9ece6a";
              yellow = "#e0af68";
              blue = "#7aa2f7";
              magenta = "#bb9af7";
              cyan = "#7dcfff";
              white = "#a9b1d6";
            };
            bright = {
              black = "#414868";
              red = "#f7768e";
              green = "#9ece6a";
              yellow = "#e0af68";
              blue = "#7aa2f7";
              magenta = "#bb9af7";
              cyan = "#7dcfff";
              white = "#c0caf5";
            };
            types = {
              fg = "#c0caf5";
              bg = "#1a1b26";
              panelbg = "#ff9e64";
              border = "#1abc9c";
              highlight = "#3d59a1";
            };
          };

          rofi = {
            bg = {
              main = "hsla(235, 18%, 12%, 1)";
              alt = "hsla(235, 18%, 12%, 0)";
              bar = "hsla(229, 24%, 18%, 1)";
            };
            fg = "hsla(228, 72%, 85%, 1)";
            ribbon = {
              outer = "hsla(188, 68%, 27%, 1)";
              inner = "hsla(202, 76%, 24%, 1)";
            };
            selected = "hsla(220, 88%, 72%, 1)";
            urgent = "hsl(349, 89%, 72%, 1)";
            transparent = "hsla(0, 0%, 0%, 0)";
          };
        };

        editor = {
          helix = {
            dark = "tokyonight";
            light = "tokyonight_storm"; # FIXME: no `tokyonight_day` as of 2022-09-01
          };
          neovim = {
            dark = "tokyonight";
            light = "tokyonight"; # TODO: vim.g.tokyonight_style = "day"
          };
          vscode = {
            dark = "Tokyo Night";
            light = "Tokyo Light";
            extension = {
              name = "tokyo-night";
              publisher = "enkia";
              version = "0.9.4";
              hash = "sha256-pKokB6446SR6LsTHyJtQ+FEA07A0W9UAI+byqtGeMGw=";
            };
          };
        };
      };

      home.configFile = let
        themeDir = "${cfg.gtk.package}/share/themes/${cfg.gtk.name}/gtk-4.0/";
      in {
        gtk4Theme-light = {
          target = "gtk-4.0/gtk.css";
          source = "${themeDir}/gtk.css";
        };
        gtk4Theme-dark = {
          target = "gtk-4.0/gtk-dark.css";
          source = "${themeDir}/gtk-dark.css";
        };
        gtk4Theme-assets = {
          target = "gtk-4.0/assets";
          source = "${themeDir}/assets";
          recursive = true;
        };
      };
    }

    (mkIf config.modules.desktop.browsers.firefox.enable {
      modules.desktop.browsers.firefox.userChrome = let
        usrChromeDir = "${config.snowflake.configDir}/firefox/userChrome";
      in
        concatMapStringsSep "\n" readFile [
          "${usrChromeDir}/treestyle-tabs.css"
        ];
    })

    (mkIf config.services.xserver.enable {
      hm.programs.rofi = {
        extraConfig = {
          icon-theme = let inherit (cfg.iconTheme) name; in "${name}";
          font = let
            inherit (cfg.font.sans) family weight size;
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

      hm.programs.sioyek.config = let
        inherit (cfg.font.mono) family size weight;
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

        "font_size" = "${toString size}";
        "ui_font" = "${family} ${weight}";
      };
    })
  ]);
}
