{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) toString;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf mkMerge;

  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.alacritty =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "OpenGL terminal emulator"; };

  config = mkIf config.modules.desktop.terminal.alacritty.enable {
    modules.shell.toolset.tmux.enable = true;

    hm.programs.alacritty = {
      enable = true;

      # https://alacritty.org/config-alacritty.html
      settings = mkMerge [
        {
          env = {
            TERM = "xterm-256color";
            WINIT_X11_SCALE_FACTOR = "1.0";
          };

          window = {
            dynamic_title = true;
            dynamic_padding = false;
            opacity = 0.8;
            decorations = "none";

            dimensions = {
              columns = 96;
              lines = 28;
            };
            position = {
              x = 50;
              y = 50;
            };
            padding = {
              x = 10;
              y = 10;
            };
          };

          scrolling = {
            history = 5000;
            multiplier = 3;
          };

          selection = {
            semantic_escape_chars = '',│`|:"' ()[]{}<>'';
            save_to_clipboard = false;
          };

          live_config_reload = true;

          shell = {
            program = "${getExe pkgs.fish}";
            args = [ "-l" "-c" "tmux new || tmux" ];
          };

          cursor = {
            style = {
              shape = "Block";
              blinking = "Off";
            };
            unfocused_hollow = true;
            thickness = 0.15;
          };

          keyboard.bindings = [
            {
              key = "N";
              mods = "Control|Shift";
              action = "SpawnNewInstance";
            }
            {
              key = "Q";
              mods = "Control";
              action = "Quit";
            }
            {
              key = "V";
              mods = "Control|Shift";
              action = "Paste";
            }
            {
              key = "C";
              mods = "Control|Shift";
              action = "Copy";
            }
            {
              key = "NumpadAdd";
              mods = "Control";
              action = "IncreaseFontSize";
            }
            {
              key = "NumpadSubtract";
              mods = "Control";
              action = "DecreaseFontSize";
            }
            {
              key = "Key0";
              mods = "Control";
              action = "ResetFontSize";
            }
          ];

          mouse = {
            hide_when_typing = true;
            bindings = [
              {
                mouse = "Middle";
                action = "PasteSelection";
              }
              {
                mouse = "Right";
                mods = "Shift";
                action = "open";
              }
            ];
          };
        }

        (mkIf (active != null) {
          import = [ "~/.config/alacritty/config/${active}.toml" ];
        })
      ];
    };

    create.configFile = mkIf (active != null) {
      alacritty-conf = {
        target = "alacritty/config/${active}.toml";
        source = let
          inherit (config.modules.themes.font) mono sans;
          inherit (config.modules.themes.colors.main) bright normal types;
          tomlFormat = pkgs.formats.toml { };
        in tomlFormat.generate "alacritty-theme" {
          font = {
            builtin_box_drawing = true;
            size = mono.size;

            normal = {
              family = "${mono.family}";
              style = "${sans.weight}";
            };

            italic = {
              family = "${mono.family}";
              style = "${sans.weight} Italic";
            };

            bold = {
              family = "${mono.family}";
              style = "${mono.weight}";
            };

            bold_italic = {
              family = "${mono.family}";
              style = "${mono.weight} Italic";
            };

            offset = {
              x = 0;
              y = 0;
            };
            glyph_offset = {
              x = 0;
              y = 0;
            };
          };

          colors = {
            primary = {
              foreground = "${types.fg}";
              background = "${types.bg}";
            };

            cursor = {
              text = "${types.bg}";
              cursor = "${normal.yellow}";
            };

            vi_mode_cursor = {
              text = "${types.bg}";
              cursor = "${normal.blue}";
            };

            selection = {
              text = "${types.bg}";
              background = "${types.highlight}";
            };

            normal = {
              black = "${normal.black}";
              red = "${normal.red}";
              green = "${normal.green}";
              yellow = "${normal.yellow}";
              blue = "${normal.blue}";
              magenta = "${normal.magenta}";
              cyan = "${normal.cyan}";
              white = "${normal.white}";
            };

            bright = {
              black = "${bright.black}";
              red = "${bright.red}";
              green = "${bright.green}";
              yellow = "${bright.yellow}";
              blue = "${bright.blue}";
              magenta = "${bright.magenta}";
              cyan = "${bright.cyan}";
              white = "${bright.white}";
            };
          };
        };
      };
    };
  };
}
