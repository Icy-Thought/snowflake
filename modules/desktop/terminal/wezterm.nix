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
  font = config.modules.themes.font;
  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.wezterm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [wezterm];

    home.configFile."wezterm/wezterm.lua" = {
      text = ''
        local wezterm = require("wezterm")

        ${optionalString (active != null) ''
          local ${active} = require("config.${active}")
        ''}

        return {
            term = "wezterm",
            audible_bell = "Disabled",
            automatically_reload_config = true,
            hide_tab_bar_if_only_one_tab = false,

            ${optionalString (active != null) "colors = ${active},"}
            bold_brightens_ansi_colors = true,
            use_fancy_tab_bar = false,

            font = wezterm.font({
                family = "${font.sans.family}",
                -- {bold = true, italic = true,}
                weight = "${font.sans.weightAlt}",
                harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
            }),

            font_size= ${toString (font.mono.size)},

            line_height = 1.0,

            window_frame = {
                active_titlebar_bg = "${colors.types.bg}",
                inactive_titlebar_bg = "${colors.black}",

                font = wezterm.font({
                    family = "${font.sans.family}",
                    weight = "${font.sans.weightAlt}",
                    style = "Italic",
                }),
                font_size= 9.0,
            },

            window_padding = {
                left = 5,
                right = 5,
                top = 0,
                bottom = 0,
            },

            text_background_opacity = 1.0,
            warn_about_missing_glyphs = false,
            window_background_opacity = 1.0,

            enable_scroll_bar = false,
            scrollback_lines = 5000,
            cursor_blink_rate = 500,

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
    };
  };
}
