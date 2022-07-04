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
          main = {
            normal = {
              black = "#242931";
              red = "#e05f65";
              green = "#78dba9";
              yellow = "#f1cf8a";
              blue = "#70a5eb";
              magenta = "#c68aee";
              cyan = "#74bee9";
              white = "#dee1e6";
            };
            bright = {
              black = "#485263";
              red = "#e05f65";
              green = "#78dba9";
              yellow = "#f1cf8a";
              blue = "#70a5eb";
              magenta = "#c68aee";
              cyan = "#74bee9";
              white = "#dee1e6";
            };
            types = {
              fg = "#b6beca";
              bg = "#171b20";
              panelbg = "#ffeba6";
              border = "#8cc1ff";
              highlight = "#94f7c5";
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
          text-color = "${normal.blue}"
          password-background-color = "${normal.black}"
          window-color = "${types.border}"
          border-color = "${types.border}"
        '';
      };
    })

    (mkIf config.modules.shell.fish.enable {
      home.configFile."fish/conf.d/${cfg.active}.fish".text = with cfg.colors.fish; ''
        # --> General
        set -l foreground ${fg}
        set -l highlight  ${highlight}

        # --> palette
        set -l base01     ${base01}
        set -l base02     ${base02}
        set -l base03     ${base03}
        set -l base04     ${base04}
        set -l base05     ${base05}
        set -l base06     ${base06}
        set -l base07     ${base07}
        set -l base08     ${base08}
        set -l base09     ${base09}
        set -l base10     ${base10}

        # Syntax Highlighting
        set -g fish_color_normal                     $foreground
        set -g fish_color_command                    $base09
        set -g fish_color_param                      $base02
        set -g fish_color_keyword                    $base05
        set -g fish_color_quote                      $base07
        set -g fish_color_redirection                $base04
        set -g fish_color_end                        $base06
        set -g fish_color_error                      $base05
        set -g fish_color_gray                       $base10
        set -g fish_color_selection    --background= $highlight
        set -g fish_color_search_match --background= $highlight
        set -g fish_color_operator                   $base04
        set -g fish_color_escape                     $base02
        set -g fish_color_autosuggestion             $base10
        set -g fish_color_cancel                     $base05

        # Prompt
        set -g fish_color_cwd                        $base08
        set -g fish_color_user                       $base01
        set -g fish_color_host                       $base09

        # Completion Pager
        set -g fish_pager_color_progress             $base10
        set -g fish_pager_color_prefix               $base04
        set -g fish_pager_color_completion           $foreground
        set -g fish_pager_color_description          $base10
      '';
    })

    (mkIf config.modules.desktop.terminal.alacritty.enable {
      home.configFile."alacritty/config/${cfg.active}.yml".text = with cfg;
        ''
          font:
            normal:
              family: "${font.sans.family}"
              style:  "${font.sans.weight}"

            bold:
              family: "${font.sans.family}"
              style:  "Bold"

            italic:
              family: "${font.sans.family}"
              style:  "${font.sans.weight} Italic"

            bold_italics:
              family: "${font.sans.family}"
              style:  "${font.sans.weight} Italic"

            size: ${toString (font.mono.size)}

            offset:
              x: 0
              y: 0

            glyph_offset:
              x: 0
              y: 0

            use_thin_strokes: true
        ''
        + (with cfg.colors.main; ''
          colors:
            primary:
              foreground: "${types.fg}"
              background: "${types.bg}"

            cursor:
              text:   "${types.bg}"
              cursor: "${normal.yellow}"

            selection:
              text:       "${types.bg}"
              background: "${types.highlight}"

            normal:
              black:      "${normal.black}"
              red:        "${normal.red}"
              green:      "${normal.green}"
              yellow:     "${normal.yellow}"
              blue:       "${normal.blue}"
              magenta:    "${normal.magenta}"
              cyan:       "${normal.cyan}"
              white:      "${normal.white}"

            bright:
              black:      "${bright.black}"
              red:        "${bright.red}"
              green:      "${bright.green}"
              yellow:     "${bright.yellow}"
              blue:       "${bright.blue}"
              magenta:    "${bright.magenta}"
              cyan:       "${bright.cyan}"
              white:      "${bright.white}"
        '');
    })

    (mkIf config.modules.desktop.terminal.kitty.enable {
      # TODO: Find ONE general nix-automation entry for VictorMono
      home.configFile."kitty/config/${cfg.active}.conf".text = with cfg;
        ''
          font_family               Victor Mono SemiBold Nerd Font Complete
          italic_font               Victor Mono SemiBold Italic Nerd Font Complete
          bold_font                 Victor Mono Bold Nerd Font Complete
          bold_italic_font          Victor Mono Bold Italic Nerd Font Complete
          font_size                 ${toString (font.mono.size)}
        ''
        + (with cfg.colors.main; ''

          foreground                ${types.fg}
          background                ${types.bg}

          cursor                    ${normal.yellow}
          cursor_text_color         ${types.bg}

          tab_bar_background        ${types.bg}
          tab_title_template        "{fmt.fg._415c6d}{fmt.bg.default} ○ {index}:{f'{title[:6]}…{title[-6:]}' if title.rindex(title[-1]) + 1 > 25 else title}{' []' if layout_name == 'stack' else '''} "
          active_tab_title_template "{fmt.fg._83b6af}{fmt.bg.default} 綠{index}:{f'{title[:6]}…{title[-6:]}' if title.rindex(title[-1]) + 1 > 25 else title}{' []' if layout_name == 'stack' else '''} "

          selection_foreground      ${types.bg}
          selection_background      ${types.highlight}

          color0                    ${normal.black}
          color8                    ${bright.black}

          color1                    ${normal.red}
          color9                    ${bright.red}

          color2                    ${normal.green}
          color10                   ${bright.green}

          color3                    ${normal.yellow}
          color11                   ${bright.yellow}

          color4                    ${normal.blue}
          color12                   ${bright.blue}

          color5                    ${normal.magenta}
          color13                   ${bright.magenta}

          color6                    ${normal.cyan}
          color14                   ${bright.cyan}

          color7                    ${normal.white}
          color15                   ${bright.white}
        '');
    })

    (mkIf config.modules.desktop.terminal.wezterm.enable {
      home.configFile."wezterm/config/${cfg.active}.lua".text = with cfg.colors.main; ''
        return {
            foreground      = "${types.fg}",
            background      = "${types.bg}",

            cursor_fg       = "${types.bg}",
            cursor_bg       = "${normal.yellow}",
            cursor_border   = "${normal.yellow}",

            selection_fg    = "${types.bg}",
            selection_bg    = "${types.highlight}",

            scrollbar_thumb = "${normal.magenta}",
            split = "${normal.green}",

            tab_bar = {
                active_tab = {
                    bg_color  = "${types.bg}",
                    fg_color  = "${normal.magenta}",
                    intensity = "Normal",
                    italic    = true,
                    underline = "Single",
                },
                inactive_tab = {
                    bg_color = "${types.bg}",
                    fg_color = "${types.fg}",
                    italic   = true,
                },
                inactive_tab_edge = "${normal.black}",
                inactive_tab_hover = {
                    bg_color  = "${types.bg}",
                    fg_color  = "${normal.yellow}",
                    italic    = true,
                    underline = "Single",
                },
                new_tab = {
                    bg_color = "${types.bg}",
                    fg_color = "${normal.green}",
                    italic   = true,
                },
                new_tab_hover = {
                    bg_color = "${types.bg}",
                    fg_color = "${normal.yellow}",
                    italic   = true,
                },
            },

            ansi = {
                "${normal.black}",
                "${normal.red}",
                "${normal.green}",
                "${normal.yellow}",
                "${normal.blue}",
                "${normal.magenta}",
                "${normal.cyan}",
                "${normal.white}",
            },

            brights = {
                "${bright.black}",
                "${bright.red}",
                "${bright.green}",
                "${bright.yellow}",
                "${bright.blue}",
                "${bright.magenta}",
                "${bright.cyan}",
                "${bright.white}",
            },
        }
      '';
    })

    (mkIf config.modules.desktop.extra.rofi.enable {
      hm.programs.rofi = {
        extraConfig = with cfg; {
          icon-theme = "${gtk.iconTheme}";
          font = "${font.sans.family} ${font.sans.weight} ${toString (font.sans.size)}";
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

    (mkIf config.modules.desktop.extra.dunst.enable {
      hm.services.dunst.settings =
        (with cfg; {
          global = {
            # Geometry
            width = 300;
            height = 200;
            origin = "top-right";
            offset = "12+48";

            padding = 20;
            horizontal_padding = 20;
            notification_limit = 0;
            separator_height = 2;

            # Progress-Bar
            progress_bar = true;
            progress_bar_height = 10;
            progress_bar_frame_width = 1;
            progress_bar_min_width = 150;
            progress_bar_max_width = 300;

            # Aesthetics
            font = "${font.sans.family} ${font.sans.weight} 11";
            frame_width = 2;
            separator_color = "frame";
            frame_color = "${colors.main.types.border}";
            transparency = 0;

            line_height = 1;
            idle_threshold = 120;
            markup = "full";
            format = "<span font='13' weight='bold'>%s</span>\\n%b";
            alignment = "left";
            vertical_alignment = "center";

            icon_position = "left";
            min_icon_size = 0;
            max_icon_size = 64;

            # Keybindings
            close = "ctrl+space";
            close_all = "ctrl+shift+space";
            history = "ctrl+grave";
            context = "ctrl+shift+period";

            mouse_left_click = "close_current";
            mouse_middle_click = "do_action, close_current";
            mouse_right_click = "close_all";
          };
        })
        // (with cfg.colors.main; {
          urgency_low = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            timeout = 5;
            #icon = /path/to/icon;
          };

          urgency_normal = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            frame_color = "${types.border}";
            timeout = 7;
            #icon = /path/to/icon;
          };

          urgency_critical = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            frame_color = "${types.error}";
            timeout = 10;
            #icon = /path/to/icon
          };
        });
    })

    (mkIf config.modules.desktop.extra.fcitx5.enable {
      home.file.".local/share/fcitx5/themes".source = pkgs.fetchFromGitHub {
        owner = "icy-thought";
        repo = "fcitx5-catppuccin";
        rev = "3b699870fb2806404e305fe34a3d2541d8ed5ef5";
        sha256 = "hOAcjgj6jDWtCGMs4Gd49sAAOsovGXm++TKU3NhZt8w=";
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

    (mkIf config.modules.desktop.media.viewer.document.enable {
      hm.programs.zathura.options =
        {
          adjust-open = "width";
          first-page-column = "1:1";
          window-title-basename = true;
          selection-clipboard = "clipboard";
          font = with cfg.font; "${sans.family} ${sans.weight} ${toString (mono.size)}";
          recolor = true;
          recolor-keephue = true;
          recolor-reverse-video = true;
        }
        // (with cfg.colors.main; {
          default-fg = "${types.fg}";
          default-bg = "${types.bg}";

          statusbar-fg = "${normal.white}";
          statusbar-bg = "${types.bg}";

          inputbar-fg = "${normal.yellow}";
          inputbar-bg = "${types.bg}";

          notification-fg = "${normal.white}";
          notification-bg = "${normal.black}";

          notification-error-fg = "${normal.white}";
          notification-error-bg = "${normal.black}";

          notification-warning-fg = "${normal.red}";
          notification-warning-bg = "${normal.black}";

          highlight-active-color = "${types.fg}";
          highlight-color = "${types.highlight}";

          completion-fg = "${normal.yellow}";
          completion-bg = "${types.bg}";

          completion-highlight-fg = "${types.bg}";
          completion-highlight-bg = "${normal.yellow}";

          recolor-lightcolor = "${types.bg}";
          recolor-darkcolor = "${normal.white}";
        });
    })
  ]);
}
