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
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./assets/nasa-jwst-blueprint.png;

        gtk = {
          name = "Catppuccin-Orange-Dark-Compact";
          package = pkgs.catppuccin-gtk.override { size = "compact"; };
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
              black = "#6e6c7e";
              red = "#f28fad";
              green = "#abe9b3";
              yellow = "#fae3b0";
              blue = "#96cdfb";
              magenta = "#f5c2e7";
              cyan = "#89dceb";
              white = "#d9e0ee";
            };
            bright = {
              black = "#988ba2";
              red = "#f28fad";
              green = "#abe9b3";
              yellow = "#fae3b0";
              blue = "#96cdfb";
              magenta = "#f5c2e7";
              cyan = "#89dceb";
              white = "#d9e0ee";
            };
            types = {
              fg = "#d9e0ee";
              bg = "#1e1d2f";
              panelbg = "#b5e8e0";
              border = "#c9cbff";
              highlight = "#f2cdcd";
            };
          };

          fish = {
            fg = "cdd6f4";
            highlight = "313244";
            base01 = "94e2d5";
            base02 = "f2cdcd";
            base03 = "cba6f7";
            base04 = "f5c2e7";
            base05 = "f38ba8";
            base06 = "fab387";
            base07 = "a6e3a1";
            base08 = "f9e2af";
            base09 = "89b4fa";
            base10 = "6c7086";
          };

          rofi = {
            bg = {
              main = "hsla(254, 25%, 10%, 1)";
              alt = "hsla(254, 25%, 10%, 0)";
              bar = "hsla(243, 24%, 15%, 1)";
            };
            fg = "hsla(220, 38%, 89%, 1)";
            ribbon = {
              outer = "hsla(41, 88%, 83%, 1)";
              inner = "hsla(23, 87%, 78%, 1)";
            };
            selected = "hsla(9, 56%, 90%, 1)";
            urgent = "hsla(342, 79%, 75%, 1)";
            transparent = "hsla(0, 0%, 0%, 0)";
          };
        };

        editor = {
          helix = {
            dark = "catppuccin_mocha";
            light = "catppuccin_latte";
          };
          neovim = {
            dark = "catppuccin";
            light = "catppuccin"; # TODO apply frappe flavour
          };
          vscode = {
            dark = "Catppuccin";
            light = "Quiet Light";
            extension = {
              name = "catppuccin-vsc";
              publisher = "Catppuccin";
              version = "1.0.6";
              hash = "sha256-4M8y8dc8BpH1yhabYJsHDT9uDWeqYjnvPBgLS+lTa5I=";
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
      fonts.fonts = with pkgs; [
        cfg.font.package
        twitter-color-emoji
      ];
    })

    (mkIf (config.modules.desktop.envProto == "x11") {
      services.xserver.displayManager = {
        lightdm.greeters.mini.extraConfig = with cfg.colors.main; ''
          text-color = "${types.bg}"
          password-background-color = "${normal.black}"
          window-color = "${types.border}"
          border-color = "${types.border}"
        '';
      };
    })

    (mkIf config.modules.desktop.extensions.fcitx5.enable {
      home.dataFile.fcitx5-theme = {
        target = "fcitx5/themes";
        source = pkgs.fetchFromGitHub {
          owner = "icy-thought";
          repo = "fcitx5-catppuccin";
          rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
          hash = "sha256-hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
        };
      };
    })

    (mkIf config.modules.desktop.toolset.docView.sioyek.enable {
      hm.programs.sioyek.config = with cfg.font; {
        "custom_background_color " = "0.12 0.11 0.18";
        "custom_text_color " = "0.85 0.88 0.93";

        "text_highlight_color" = "0.85 0.88 0.93";
        "visual_mark_color" = "0.27 0.28 0.35 1.0";
        "search_highlight_color" = "0.95 0.55 0.66";
        "link_highlight_color" = "0.59 0.80 0.98";
        "synctex_highlight_color" = "0.96 0.88 0.86";

        "page_separator_width" = "2";
        "page_separator_color" = "0.95 0.80 0.80";
        "status_bar_color" = "0.19 0.20 0.27";

        "font_size" = "${toString (mono.size)}";
        "ui_font" = "${mono.family} ${mono.weight}";
      };
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with cfg.vscode.extension;
        pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
          name = "${name}";
          publisher = "${publisher}";
          version = "${version}";
          hash = "${hash}";
        }];
    })

    (mkIf config.modules.desktop.extensions.rofi.enable {
      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${iconTheme.name}";
          font = with font; "${sans.family} ${sans.weight} ${toString (sans.size)}";
        };

        theme =
          let
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
