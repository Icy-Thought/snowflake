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
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    {
      modules.themes = {
        wallpaper = mkDefault ./wallpaper.png;

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
          black = "#6e6c7e";
          red = "#f28fad";
          green = "#abe9b3";
          yellow = "#fae3b0";
          blue = "#96cdfb";
          magenta = "#f5c2e7";
          cyan = "#89dceb";
          white = "#d9e0ee";

          brightBlack = "#988ba2";
          brightRed = "#f28fad";
          brightGreen = "#abe9b3";
          brightYellow = "#fae3b0";
          brightBlue = "#96cdfb";
          brightMagenta = "#f5c2e7";
          brightCyan = "#89dceb";
          brightWhite = "#d9e0ee";

          types = {
            fg = "#d9e0ee";
            bg = "#1e1d2f";
            panelbg = "#b5e8e0";
            border = "#c9cbff";
            highlight = "#f2cdcd";
          };
        };

        neovim.theme = "catppuccin";

        vscode = {
          extension = {
            name = "catppuccin-vsc";
            publisher = "Catppuccin";
            version = "1.0.6";
            sha256 = "4M8y8dc8BpH1yhabYJsHDT9uDWeqYjnvPBgLS+lTa5I=";
          };

          theme = {
            dark = "Catppuccin";
            light = "Quiet Light";
          };
        };

        fish.colors = {
          fg = "#cdd6f4";
          highlight = "#313244";
          base01 = "#94e2d5";
          base02 = "#f2cdcd";
          base03 = "#cba6f7";
          base04 = "#f5c2e7";
          base05 = "#f38ba8";
          base06 = "#fab387";
          base07 = "#a6e3a1";
          base08 = "#f9e2af";
          base09 = "#89b4fa";
          base10 = "#6c7086";
        };

        rofi.colors = {
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

          highlight = "hsla(9, 56%, 90%, 1)";
          urgent = "hsla(342, 79%, 75%, 1)";
          transparent = "hsla(0, 0%, 0%, 0)";
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

    (mkIf (config.modules.desktop.xmonad.enable || config.modules.desktop.qtile.enable) {
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
    })

    (mkIf config.modules.shell.fish.enable {
      home.configFile."fish/conf.d/${cfg.active}.fish".text = ''
        # --> General
        set -l foreground ${cfg.colors.fg}
        set -l highlight  ${cfg.colors.highlight}

        # --> palette
        set -l base01     ${cfg.colors.base01}
        set -l base02     ${cfg.colors.base02}
        set -l base03     ${cfg.colors.base03}
        set -l base04     ${cfg.colors.base04}
        set -l base05     ${cfg.colors.base05}
        set -l base06     ${cfg.colors.base06}
        set -l base07     ${cfg.colors.base07}
        set -l base08     ${cfg.colors.base08}
        set -l base09     ${cfg.colors.base09}
        set -l base10     ${cfg.colors.base10}

        # Syntax Highlighting
        set -g fish_color_normal            $foreground
        set -g fish_color_command           $base09
        set -g fish_color_param             $base02
        set -g fish_color_keyword           $base05
        set -g fish_color_quote             $base07
        set -g fish_color_redirection       $base04
        set -g fish_color_end               $base06
        set -g fish_color_error             $base05
        set -g fish_color_gray              $base10
        set -g fish_color_selection         --background=$highlight
        set -g fish_color_search_match      --background=$highlight
        set -g fish_color_operator          $base04
        set -g fish_color_escape            $base02
        set -g fish_color_autosuggestion    $base10
        set -g fish_color_cancel            $base05

        # Prompt
        set -g fish_color_cwd               $base08
        set -g fish_color_user              $base01
        set -g fish_color_host              $base09

        # Completion Pager
        set -g fish_pager_color_progress    $base10
        set -g fish_pager_color_prefix      $base04
        set -g fish_pager_color_completion  $foreground
        set -g fish_pager_color_description $base10
      '';
    })

    (mkIf config.modules.desktop.terminal.alacritty.enable {
      home.configFile."alacritty/config/${cfg.active}.yml".text = ''
        font:
          normal:
            family: "${cfg.font.sans.family}"
            style:  "${cfg.font.sans.weight}"

          bold:
            family: "${cfg.font.sans.family}"
            style:  "Bold"

          italic:
            family: "${cfg.font.sans.family}"
            style:  "${cfg.font.sans.weight} Italic"

          bold_italics:
            family: "${cfg.font.sans.family}"
            style:  "${cfg.font.sans.weight} Italic"

          size: ${toString (cfg.font.mono.size)}

          offset:
            x: 0
            y: 0

          glyph_offset:
            x: 0
            y: 0

          use_thin_strokes: true

        colors:
          primary:
            foreground: "${cfg.colors.types.fg}"
            background: "${cfg.colors.types.bg}"

          cursor:
            text:   "${cfg.colors.types.bg}"
            cursor: "${cfg.colors.yellow}"

          selection:
            text:       "${cfg.colors.types.bg}"
            background: "${cfg.colors.types.highlight}"

          normal:
            black:      "${cfg.colors.black}"
            red:        "${cfg.colors.red}"
            green:      "${cfg.colors.green}"
            yellow:     "${cfg.colors.yellow}"
            blue:       "${cfg.colors.blue}"
            magenta:    "${cfg.colors.magenta}"
            cyan:       "${cfg.colors.cyan}"
            white:      "${cfg.colors.white}"

          bright:
            black:      "${cfg.colors.brightBlack}"
            red:        "${cfg.colors.brightRed}"
            green:      "${cfg.colors.brightGreen}"
            yellow:     "${cfg.colors.brightYellow}"
            blue:       "${cfg.colors.brightBlue}"
            magenta:    "${cfg.colors.brightMagenta}"
            cyan:       "${cfg.colors.brightCyan}"
            white:      "${cfg.colors.brightWhite}"
      '';
    })

    (mkIf config.modules.desktop.terminal.kitty.enable {
      # TODO: Find ONE general nix-automation entry for VictorMono
      "kitty/config/${cfg.active}.conf".text = ''
        font_family               Victor Mono SemiBold Nerd Font Complete
        italic_font               Victor Mono SemiBold Italic Nerd Font Complete
        bold_font                 Victor Mono Bold Nerd Font Complete
        bold_italic_font          Victor Mono Bold Italic Nerd Font Complete
        font_size                 ${toString (cfg.font.mono.size)}

        foreground                ${cfg.colors.types.fg}
        background                ${cfg.colors.types.bg}

        cursor                    ${cfg.colors.yellow}
        cursor_text_color         ${cfg.colors.types.bg}

        tab_bar_background        ${cfg.colors.types.bg}
        tab_title_template        "{fmt.fg._415c6d}{fmt.bg.default} ○ {index}:{f'{title[:6]}…{title[-6:]}' if title.rindex(title[-1]) + 1 > 25 else title}{' []' if layout_name == 'stack' else '''} "
        active_tab_title_template "{fmt.fg._83b6af}{fmt.bg.default} 綠{index}:{f'{title[:6]}…{title[-6:]}' if title.rindex(title[-1]) + 1 > 25 else title}{' []' if layout_name == 'stack' else '''} "

        selection_foreground      ${cfg.colors.types.bg}
        selection_background      ${cfg.colors.types.highlight}

        color0                    ${cfg.colors.black}
        color8                    ${cfg.colors.brightBlack}

        color1                    ${cfg.colors.red}
        color9                    ${cfg.colors.brightRed}

        color2                    ${cfg.colors.green}
        color10                   ${cfg.colors.brightGreen}

        color3                    ${cfg.colors.yellow}
        color11                   ${cfg.colors.brightYellow}

        color4                    ${cfg.colors.blue}
        color12                   ${cfg.colors.brightBlue}

        color5                    ${cfg.colors.magenta}
        color13                   ${cfg.colors.brightMagenta}

        color6                    ${cfg.colors.cyan}
        color14                   ${cfg.colors.brightCyan}

        color7                    ${cfg.colors.white}
        color15                   ${cfg.colors.brightWhite}
      '';
    })

    (mkIf config.modules.desktop.terminal.wezterm.enable {
      "wezterm/config/${cfg.active}.lua".text = ''
        return {
            foreground      = "${cfg.colors.types.fg}",
            background      = "${cfg.colors.types.bg}",

            cursor_fg       = "${cfg.colors.types.bg}",
            cursor_bg       = "${cfg.colors.yellow}",
            cursor_border   = "${cfg.colors.yellow}",

            selection_fg    = "${cfg.colors.types.bg}",
            selection_bg    = "${cfg.colors.types.highlight}",

            scrollbar_thumb = "${cfg.colors.magenta}",
            split = "${cfg.colors.green}",

            tab_bar = {
                active_tab = {
                    bg_color  = "${cfg.colors.types.bg}",
                    fg_color  = "${cfg.colors.magenta}",
                    intensity = "Normal",
                    italic    = true,
                    underline = "Single",
                },
                inactive_tab = {
                    bg_color = "${cfg.colors.types.bg}",
                    fg_color = "${cfg.colors.types.fg}",
                    italic   = true,
                },
                inactive_tab_edge = "${cfg.colors.black}",
                inactive_tab_hover = {
                    bg_color  = "${cfg.colors.types.bg}",
                    fg_color  = "${cfg.colors.yellow}",
                    italic    = true,
                    underline = "Single",
                },
                new_tab = {
                    bg_color = "${cfg.colors.types.bg}",
                    fg_color = "${cfg.colors.green}",
                    italic   = true,
                },
                new_tab_hover = {
                    bg_color = "${cfg.colors.types.bg}",
                    fg_color = "${cfg.colors.yellow}",
                    italic   = true,
                },
            },

            ansi = {
                "${cfg.colors.black}",
                "${cfg.colors.red}",
                "${cfg.colors.green}",
                "${cfg.colors.yellow}",
                "${cfg.colors.blue}",
                "${cfg.colors.magenta}",
                "${cfg.colors.cyan}",
                "${cfg.colors.white}",
            },

            brights = {
                "${cfg.colors.brightBlack}",
                "${cfg.colors.brightRed}",
                "${cfg.colors.brightGreen}",
                "${cfg.colors.brightYellow}",
                "${cfg.colors.brightBlue}",
                "${cfg.colors.brightMagenta}",
                "${cfg.colors.brightCyan}",
                "${cfg.colors.brightWhite}",
            },
        }
      '';
    })

    (mkIf config.modules.desktop.extra.rofi.enable {
      home.programs.rofi = {
        extraConfig = {
          icon-theme = "${cfg.gtk.iconTheme}";
          font = "${cfg.font.sans.family} ${cfg.font.sans.weight} ${toString (cfg.font.sans.size)}";
        };

        theme = let
          inherit (config.lib.formats.rasi) mkLiteral;
        in {
          "*" = {
            fg = mkLiteral "${cfg.rofi.colors.fg}";
            bg = mkLiteral "${cfg.rofi.colors.bg}";
            bg-alt = mkLiteral "${cfg.rofi.colors.bg.alt}";
            bg-bar = mkLiteral "${cfg.rofi.colors.bg.bar}";

            outer-ribbon = mkLiteral "${cfg.rofi.colors.ribbon.outer}";
            inner-ribbon = mkLiteral "${cfg.rofi.colors.ribbon.inner}";
            highlight = mkLiteral "${cfg.rofi.colors.highlighted}";
            urgent = mkLiteral "${cfg.rofi.colors.urgent}";
            transparent = mkLiteral "${cfg.rofi.colors.transparent}";
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
            x-offset = mkLiteral 0;
            y-offset = mkLiteral 0;
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
            horizontal-align = 0.5;
            vertical-align = 0.5;
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
            background-color = mkLiteral "@highlight";
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

    (mkif config.modules.desktop.extra.dunst.enable {
      home.services.dunst.settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          indicate_hidden = "yes";

          # Geometry
          width = 300;
          height = 200;
          origin = "top-right";
          offset = "12+48";

          # Notification
          sort = "yes";
          scale = 0;
          shrink = "no";
          word_wrap = "yes";

          padding = 20;
          horizontal_padding = 20;
          notification_limit = 0;
          separator_height = 2;
          stack_duplicates = true;
          hide_duplicate_count = false;

          # Progress-Bar
          progress_bar = true;
          progress_bar_height = 10;
          progress_bar_frame_width = 1;
          progress_bar_min_width = 150;
          progress_bar_max_width = 300;

          # Aesthetics
          transparency = 0;
          frame_width = 2;
          frame_color = "${cfg.colors.types.border}";
          separator_color = "frame";
          font = "${cfg.font.sans.family} ${cfg.font.sans.weight} 11";

          line_height = 1;
          idle_threshold = 120;
          markup = "full";
          format = "<span font='13' weight='bold'>%s</span>\\n%b";
          alignment = "left";
          vertical_alignment = "center";

          icon_position = "left";
          min_icon_size = 0;
          max_icon_size = 64;

          # General
          title = "Dunst";
          class = "Dunst";

          show_age_threshold = 60;
          ellipsize = "middle";
          ignore_newline = "no";
          show_indicators = "no";
          sticky_history = "no";
          history_length = 20;

          browser = "firefox-devedition";
          always_run_script = true;
          ignore_dbusclose = false;
          force_xinerama = false;

          # Keybindings
          close = "ctrl+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+grave";
          context = "ctrl+shift+period";

          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
        };

        experimental = {
          per_monitor_dpi = false;
        };

        urgency_low = {
          foreground = "${cfg.colors.types.fg}";
          background = "${cfg.colors.types.bg}";
          timeout = 5;
          #icon = /path/to/icon;
        };

        urgency_normal = {
          foreground = "${cfg.colors.types.fg}";
          background = "${cfg.colors.types.bg}";
          frame_color = "${cfg.colors.types.border}";
          timeout = 7;
          #icon = /path/to/icon;
        };

        urgency_critical = {
          foreground = "${cfg.colors.types.fg}";
          background = "${cfg.colors.types.bg}";
          frame_color = "${cfg.colors.types.error}";
          timeout = 10;
          #icon = /path/to/icon
        };

        fullscreen_pushback_everything = {
          fullscreen = "pushback";
        };
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

    (mkIf config.modules.desktop.editors.vscodium.enable {
      home.programs.vscode.extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "${cfg.vscode.extension.name}";
          publisher = "${cfg.vscode.extension.publisher}";
          version = "${cfg.vscode.extension.version}";
          sha256 = "${cfg.vscode.extension.sha256}";
        }
      ];
    })

    (mkIf config.modules.desktop.media.viewer.document {
      home.programs.zathura.options = {
        adjust-open = "width";
        first-page-column = "1:1";
        window-title-basename = true;
        selection-clipboard = "clipboard";
        font = "${cfg.font.sans.family} ${cfg.font.sans.weight} ${toString (cfg.font.mono.size)}";

        default-fg = "${cfg.colors.types.fg}";
        default-bg = "${cfg.colors.types.bg}";

        statusbar-fg = "${cfg.colors.white}";
        statusbar-bg = "${cfg.colors.types.bg}";

        inputbar-fg = "${cfg.colors.yellow}";
        inputbar-bg = "${cfg.colors.types.bg}";

        notification-fg = "${cfg.colors.white}";
        notification-bg = "${cfg.colors.black}";

        notification-error-fg = "${cfg.colors.white}";
        notification-error-bg = "${cfg.colors.black}";

        notification-warning-fg = "${cfg.colors.red}";
        notification-warning-bg = "${cfg.colors.black}";

        highlight-active-color = "${cfg.colors.types.fg}";
        highlight-color = "${cfg.colors.types.highlight}";

        completion-fg = "${cfg.colors.yellow}";
        completion-bg = "${cfg.colors.types.bg}";

        completion-highlight-fg = "${cfg.colors.types.bg}";
        completion-highlight-bg = "${cfg.colors.yellow}";

        recolor-lightcolor = "${cfg.colors.types.bg}";
        recolor-darkcolor = "${cfg.colors.white}";

        recolor = true;
        recolor-keephue = true;
        recolor-reverse-video = true;
      };
    })
  ]);
}
