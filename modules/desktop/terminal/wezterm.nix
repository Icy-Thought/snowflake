{ config
, options
, lib
, pkgs
, ...
}:

let inherit (builtins) toString;
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.desktop.terminal.wezterm = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.terminal.wezterm.enable {
    user.packages = with pkgs; [ wezterm ];

    home.configFile = with config.modules.themes;
      let wezDir = "${config.snowflake.configDir}/wezterm";
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

              ${strings.optionalString (active != null) ''
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

              local ${active} = require("theme.${active}")

              M.colors = ${active}

              return M
            '';
          };

          wezterm-font = {
            target = "wezterm/theme/font.lua";
            text = ''
              local wez = require("wezterm")

              local M = {}

              M.font = wez.font_with_fallback({ 
                { family = "${font.sans.family}",
                  weight = "${font.sans.weightAlt}",
                },
                "DejaVu Sans",
                "Unicode",
              })

              M.font_size = ${toString (font.mono.size)}
              M.char_select_font_size = ${toString (font.mono.size)}

              M.window_frame = {
                active_titlebar_bg = "${colors.main.types.bg}",
                inactive_titlebar_bg = "${colors.main.normal.black}",

                font = wez.font({
                    family = "${font.sans.family}",
                    weight = "${font.sans.weightAlt}",
                    style = "Italic",
                }),

                font_size= ${toString (font.mono.size)},
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
            text = with colors.main; ''
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
