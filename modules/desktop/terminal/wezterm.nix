{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.terminal.wezterm;
  configDir = config.snowflake.configDir;
  themeCfg = config.modules.themes;
in {
  options.modules.desktop.terminal.wezterm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [wezterm];

    home.configFile = with themeCfg;
      mkMerge [
        {
          "wezterm/wezterm.lua".text = ''
            local wezterm = require("wezterm")
            ${optionalString (active != null) ''
              local ${active} = require("config.${active}")
              require("config.${active}-bar")
            ''}

            return {
                term = "wezterm",
                audible_bell = "Disabled",
                automatically_reload_config = true,
                hide_tab_bar_if_only_one_tab = false,

                bold_brightens_ansi_colors = true,
                use_fancy_tab_bar = false,
                line_height = 1.0,

            ${optionalString (active != null) ''
              colors = ${active},

              font = wezterm.font({
                  family = "${font.sans.family}",
                  -- {bold = true, italic = true,}
                  weight = "${font.sans.weightAlt}",
                  harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
              }),

              font_size= ${toString (font.mono.size)},

              window_frame = {
                  active_titlebar_bg = "${colors.main.types.bg}",
                  inactive_titlebar_bg = "${colors.main.normal.black}",

                  font = wezterm.font({
                      family = "${font.sans.family}",
                      weight = "${font.sans.weightAlt}",
                      style = "Italic",
                  }),
                  font_size= 9.0,
              },
            ''}

                window_padding = {
                    left = 15,
                    right = 15,
                    top = 0,
                    bottom = 0,
                },

                text_background_opacity = 1.0,
                warn_about_missing_glyphs = false,
                window_background_opacity = 1.0,
                enable_scroll_bar = false,
                scrollback_lines = 5000,

                default_cursor_style = "BlinkingBlock",
                cursor_blink_rate = 500,
                cursor_blink_ease_in = "Constant",
                cursor_blink_ease_out = "Constant",

                hyperlink_rules = {
                    { -- This is actually the default if you don't specify any hyperlink_rules
                        regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
                        format = "$0",
                    },
                    { -- linkify email addresses
                        regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
                        format = "mailto:$0",
                    },
                    { -- file:// URI
                        regex = "\\bfile://\\S*\\b",
                        format = "$0",
                    },
                    { -- nixpkgs review current program
                        regex = "nixpkgs-review pr (\\d+)",
                        format = "https://github.com/NixOS/nixpkgs/pull/$1",
                    },
                    {
                        regex = "pr-(\\d+)-?\\d?",
                        format = "https://github.com/NixOS/nixpkgs/pull/$1",
                    },
                    { -- nix flake github references
                        regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)",
                        format = "https://github.com/$1/$2",
                    },
                    { -- nix flake github references with commit
                        regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)/([\\d\\w-]+)",
                        format = "https://github.com/$1/$2/commit/$3",
                    },
                    { -- git ssh remote url
                        regex = "git@(\\w+\\.\\w+):(\\w+/\\w+)\\.git",
                        format = "https://$1/$2",
                    },
                },
                keys = {
                    { -- scroll between prompts with Shift+Arrow up/down
                        key = "UpArrow",
                        mods = "SHIFT",
                        action = wezterm.action({ ScrollToPrompt = -1 }),
                    },
                    {
                        key = "DownArrow",
                        mods = "SHIFT",
                        action = wezterm.action({ ScrollToPrompt = 1 }),
                    },
                    { -- split current pane vertical
                        key = '"',
                        mods = "CTRL|SHIFT|ALT",
                        action = wezterm.action({
                            SplitVertical = { domain = "CurrentPaneDomain" },
                        }),
                    },
                    { -- split current pane horizontal
                        key = "%",
                        mods = "CTRL|SHIFT|ALT",
                        action = wezterm.action({
                            SplitHorizontal = { domain = "CurrentPaneDomain" },
                        }),
                    },
                },
            }
          '';
        }

        (mkIf (active != null) {
          "wezterm/config/${active}-bar.lua" = {
            source = "${configDir}/wezterm/${active}-bar.lua";
          };

          "wezterm/config/${active}.lua".text = with colors.main; ''
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
        })
      ];
  };
}
