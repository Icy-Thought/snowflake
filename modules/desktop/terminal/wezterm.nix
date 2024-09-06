{ config, options, lib, pkgs, ... }:
let
  inherit (builtins) toString;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) optionalString;
in {
  options.modules.desktop.terminal.wezterm =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "GPU-accelerated terminal emulator"; };

  config = mkIf config.modules.desktop.terminal.wezterm.enable {
    user.packages = [ pkgs.wezterm ];

    create.configFile = let
      inherit (config.modules.themes) active;
      wezDir = "${config.snowflake.configDir}/wezterm";
    in mkMerge [
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

            ${optionalString (active != null) ''
              require("statusbar.${active}")

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

      (mkIf (active != null) {
        wezterm-rice = {
          target = "wezterm/theme/colorscheme.lua";
          text = ''
            local M = {}

            local active_palette = require("theme.${active}")

            M.colors = active_palette

            return M
          '';
        };

        wezterm-font = {
          target = "wezterm/theme/font.lua";
          text = let
            inherit (config.modules.themes.font.mono) family size weightAlt;
            inherit (config.modules.themes.colors.main) types normal;
          in ''
            local wez = require("wezterm")

            local M = {}

            M.font = wez.font_with_fallback({
              { family = "${family}",
                weight = "${weightAlt}",
              },
              "DejaVu Sans",
              "Unicode",
            })

            M.font_size = ${toString size}
            M.char_select_font_size = ${toString size}

            M.window_frame = {
              active_titlebar_bg = "${types.bg}",
              inactive_titlebar_bg = "${normal.black}",

              font = wez.font({
                  family = "${family}",
                  weight = "${weightAlt}",
                  style = "Italic",
              }),

              font_size= ${toString size},
            }

            return M
          '';
        };

        wezterm-statusbar = {
          target = "wezterm/statusbar/${active}.lua";
          source = "${wezDir}/statusbar/${active}.lua";
        };

        wezterm-theme = {
          target = "wezterm/theme/${active}.lua";
          text =
            let inherit (config.modules.themes.colors.main) bright normal types;
            in ''
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
