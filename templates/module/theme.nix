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
  config = mkIf (cfg.active == "") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./wallpaper.png;

        gtk = {
          theme = "";
          iconTheme = "";
          cursor = {
            name = "";
            size = 24;
          };
        };

        font = {
          sans.family = "";
          mono.family = "";
          emoji = "";
        };

        colors = {
          main = {
            normal = {
              black = "";
              red = "";
              green = "";
              yellow = "";
              blue = "";
              magenta = "";
              cyan = "";
              white = "";
            };
            bright = {
              black = "";
              red = "";
              green = "";
              yellow = "";
              blue = "";
              magenta = "";
              cyan = "";
              white = "";
            };
            types = {
              fg = "";
              bg = "";
              panelbg = "";
              border = "";
              highlight = "";
            };
          };

          fish = {
            fg = "";
            highlight = "";
            base01 = "";
            base02 = "";
            base03 = "";
            base04 = "";
            base05 = "";
            base06 = "";
            base07 = "";
            base08 = "";
            base09 = "";
            base10 = "";
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

        neovim.theme = "";

        vscode = {
          extension = {
            name = "";
            publisher = "";
            version = "";
            hash = "";
          };
          theme = {
            dark = "";
            light = "";
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
      user.packages = with cfg.gtk; [
        theme-package # WARN:
        icon-package # WARN:
        cursor-package # WARN:
      ];

      fonts.fonts = with pkgs; [
        victor-mono
        emoji-theme # WARN:
      ];
    })

    (mkIf
      (config.modules.desktop.xmonad.enable
        || config.modules.desktop.qtile.enable)
      {
        services.xserver.displayManager = {
          sessionCommands = with cfg.gtk; ''
            ${getExe pkgs.xorg.xsetroot} -xcf ${pkgs.cursor-package}/share/icons/${cursor.name}/cursors/${cursor.default} ${toString (cursor.size)}
          ''; # WARN:

          # LightDM: Replace with LightDM-Web-Greeter theme
          lightdm.greeters.mini.extraConfig = with cfg.colors.main; ''
            text-color = "${types.bg}"
            password-background-color = "${normal.black}"
            window-color = "${types.border}"
            border-color = "${types.border}"
          '';
        };
      })

    (mkIf config.modules.desktop.extra.fcitx5.enable {
      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "";
        repo = "";
        rev = "";
        hash = "";
      }; # WARN:
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
      }; # WARN:
    })

    (mkIf config.modules.desktop.editors.vscodium.enable {
      hm.programs.vscode.extensions = with cfg.vscode.extension;
        pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "${name}";
            publisher = "${publisher}";
            version = "${version}";
            hash = "${hash}";
          }
        ];
    })

    (mkIf config.modules.desktop.extra.rofi.enable {
      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${gtk.iconTheme}";
          font = "${font.sans.family} ${font.sans.weight} ${toString (font.sans.size)}";
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
