{ options, config, lib, pkgs, ... }:

let
  activeTheme = config.modules.themes.active;
  wezDir = "${config.snowflake.configDir}/wezterm";
in with lib; {
  options.modules.desktop.terminal.wezterm = {
    enable = mkEnableOption "GPU-accelerated terminal emulator";
  };

  config = mkIf config.modules.desktop.terminal.wezterm.enable {
    user.packages = [ pkgs.wezterm ];

    create.configFile = mkMerge [
      {
        wezterm-utils = {
          target = "wezterm/utils.lua";
          source = "${wezDir}/utils/init.lua";
        };

        wezterm-init = {
          target = "wezterm/wezterm.lua";
          text = ''
            require("modules.custom-title")

            local config = require("modules.config")
            local keys = require("modules.keys")
            local mouse = require("modules.mouse")
            local hyperlinks = require("modules.hyperlinks")

            ${optionalString (activeTheme != null) ''
              require("statusbar.${activeTheme}")

              local colorscheme = require("theme.colorscheme")
              local font = require("theme.font")
            ''}

            local merge = require("utils").merge_conf

            return merge(config, colorscheme, font, keys, mouse, hyperlinks)
          '';
        };

        wezterm-conf = {
          target = "wezterm/modules/config.lua";
          source = "${wezDir}/modules/config.lua";
        };

        wezterm-custom-title = {
          target = "wezterm/modules/custom-title.lua";
          source = "${wezDir}/modules/custom-title.lua";
        };

        wezterm-keybindings = {
          target = "wezterm/modules/keys.lua";
          source = "${wezDir}/modules/keys.lua";
        };

        wezterm-mouse-bindings = {
          target = "wezterm/modules/mouse.lua";
          source = "${wezDir}/modules/mouse.lua";
        };

        wezterm-hyperlinks = {
          target = "wezterm/modules/hyperlinks.lua";
          source = "${wezDir}/modules/hyperlinks.lua";
        };
      }

      (mkIf (activeTheme != null) {
        wezterm-rice = {
          target = "wezterm/theme/colorscheme.lua";
          text = ''
            local M = {}

            local active_palette = require("theme.${activeTheme}")

            M.colors = active_palette

            return M
          '';
        };

        wezterm-font = {
          target = "wezterm/theme/font.lua";
          text = with config.modules.themes.font.mono; ''
            local wez = require("wezterm")

            local M = {}

            M.font = wez.font_with_fallback({
              { family = "${family}",
                weight = "${weightAlt}",
              },
              "DejaVu Sans",
              "Unicode",
            })

            M.font_size = ${builtins.toString size}
            M.char_select_font_size = ${builtins.toString size}

            M.window_frame = {
              font = wez.font({
                  family = "${family}",
                  weight = "${weightAlt}",
                  style = "Italic",
              }),

              font_size= ${builtins.toString size},
            }

            return M
          '';
        };

        wezterm-statusbar = {
          target = "wezterm/statusbar/${activeTheme}.lua";
          source = "${wezDir}/statusbar/${activeTheme}.lua";
        };

        wezterm-theme = {
          target = "wezterm/theme/${activeTheme}.lua";
          text = with config.modules.themes.colors.main; ''
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
                    background = "${types.bg}",
                    active_tab = {
                        bg_color  = "${types.bg}",
                        fg_color  = "${normal.magenta}",
                        intensity = "Normal",
                        italic    = true,
                    },
                    inactive_tab = {
                        bg_color = "${types.bg}",
                        fg_color = "${types.fg}",
                    },
                    inactive_tab_edge = "${normal.black}",
                    inactive_tab_hover = {
                        bg_color  = "${types.bg}",
                        fg_color  = "${normal.yellow}",
                        underline = "Single",
                    },
                    new_tab = {
                        bg_color = "${types.bg}",
                        fg_color = "${normal.green}",
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
        };
      })
    ];
  };
}
