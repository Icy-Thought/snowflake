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
  fishCfg = "${config.snowflake.configDir}/fish";
  configDir = config.snowflake.configDir;
  themeCfg = config.modules.themes;
in {
  options.modules.desktop.terminal.wezterm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [wezterm];

    hm.programs.fish = {
      interactiveShellInit = ''
        ${builtins.readFile "${fishCfg}/abbreviations/wezterm.fish"}
      '';
    };

    home.configFile = with themeCfg;
      mkMerge [
        {
          "wezterm/wezterm.lua".text = ''
            local wezterm = require("wezterm")
            ${optionalString (active != null) ''
              local ${active} = require("themes.${active}")
              require("statusbar.${active}")
            ''}

            local key_bindings = {
                { -- Focus -> left window
                    key = "j",
                    mods = "ALT",
                    action = wezterm.action { ActivatePaneDirection = "Left" }
                }, { -- Focus -> right window
                    key = "k",
                    mods = "ALT",
                    action = wezterm.action { ActivatePaneDirection = "Right" }
                }, { -- Cycle prompts -> left
                    key = "h",
                    mods = "ALT",
                    action = wezterm.action({ ActivateTabRelative = -1 }),
                }, { -- Cycle prompts -> right
                    key = "l",
                    mods = "ALT",
                    action = wezterm.action({ ActivateTabRelative = 1 }),
                }, { -- Split pane -> vertically
                    key = '|',
                    mods = "CTRL|SHIFT",
                    action = wezterm.action({
                        SplitVertical = { domain = "CurrentPaneDomain" },
                    }),
                }, {
                    key = "-",
                    mods = "CTRL|SHIFT",
                    action = wezterm.action({
                        SplitHorizontal = { domain = "CurrentPaneDomain" },
                    }),
                },
            }

            local mouse_bindings = {
                {
                    event = { Down = { streak = 1, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { SelectTextAtMouseCursor = "Cell" }
                }, {
                    event = { Down = { streak = 2, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { SelectTextAtMouseCursor = "Word" }
                }, {
                    event = { Down = { streak = 3, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { SelectTextAtMouseCursor = "Line" }
                }, {
                    event = { Drag = { streak = 1, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { ExtendSelectionToMouseCursor = "Cell" }
                }, {
                    event = { Drag = { streak = 2, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { ExtendSelectionToMouseCursor = "Word" }
                }, {
                    event = { Drag = { streak = 3, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { ExtendSelectionToMouseCursor = "Line" }
                }, {
                    event = { Up = { streak = 1, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action {
                        CompleteSelection = "ClipboardAndPrimarySelection"
                    }
                }, {
                    event = { Up =  { streak = 2, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { CompleteSelection = "PrimarySelection" }
                }, {
                    event = { Up = { streak = 3, button = "Left" } },
                    mods = "NONE",
                    action = wezterm.action { CompleteSelection = "PrimarySelection" }
                }, {
                    event = { Up = { streak = 1, button = "Left" } },
                    mods = "SUPER",
                    action = wezterm.action {
                        CompleteSelectionOrOpenLinkAtMouseCursor = "ClipboardAndPrimarySelection"
                    }
                }
            }

            local hyperlink_rules = {
                { -- This is actually the default if you don't specify any hyperlink_rules
                        regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
                        format = "$0",
                }, { -- linkify email addresses
                    regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
                    format = "mailto:$0",
                }, { -- file:// URI
                    regex = "\\bfile://\\S*\\b",
                    format = "$0",
                }, { -- nixpkgs review current program
                    regex = "nixpkgs-review pr (\\d+)",
                    format = "https://github.com/NixOS/nixpkgs/pull/$1",
                }, {
                    regex = "pr-(\\d+)-?\\d?",
                    format = "https://github.com/NixOS/nixpkgs/pull/$1",
                }, { -- nix flake github references
                    regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)",
                    format = "https://github.com/$1/$2",
                }, { -- nix flake github references with commit
                    regex = "github:([\\w\\d_-]+)/([\\w\\d_\\.-]+)/([\\d\\w-]+)",
                    format = "https://github.com/$1/$2/commit/$3",
                }, { -- git ssh remote url
                    regex = "git@(\\w+\\.\\w+):(\\w+/\\w+)\\.git",
                    format = "https://$1/$2",
                },
            }

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

              font = wezterm.font_with_fallback({
                  { family = "${font.sans.family}", weight = "${font.sans.weightAlt}" },
                  "DejaVu Sans",
                  "Unicode",
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
                  font_size= ${toString (font.mono.size)},
              },
            ''}

                window_padding = {
                    left = 15,
                    right = 15,
                    top = 10,
                    bottom = 0,
                },

                text_background_opacity = 1.0,
                warn_about_missing_glyphs = false,
                window_background_opacity = 0.75,
                enable_scroll_bar = false,
                scrollback_lines = 5000,

                cursor_blink_rate = 500,
                cursor_blink_ease_in = "Constant",
                cursor_blink_ease_out = "Constant",
                default_cursor_style = "BlinkingBlock",

                hyperlink_rules = hyperlink_rules,
                keys = key_bindings,
                mouse_bindings = mouse_bindings,
            }
          '';
        }

        (mkIf (active != null) {
          "wezterm/statusbar/${active}.lua" = {
            source = "${configDir}/wezterm/statusbar/${active}.lua";
          };

          "wezterm/themes/${active}.lua".text = with colors.main; ''
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
