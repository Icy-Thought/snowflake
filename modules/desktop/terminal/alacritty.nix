{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) toString;
  inherit (lib) mkIf mkMerge getExe;
  inherit (lib.my) mkBoolOpt;

  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.alacritty = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.terminal.alacritty.enable {
    # Enabling useful/configured term-tools:
    modules.shell.tmux.enable = true;

    hm.programs.alacritty = {
      enable = true;

      settings = (mkMerge [
        {
          env = {
            TERM = "xterm-256color";
            WINIT_X11_SCALE_FACTOR = "1.0";
          };

          window = {
            dynamic_title = true;
            dynamic_padding = false;
            opacity = 0.7;
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
            faux_multiplier = 3;
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
            style = "Block";
            unfocused_hollow = true;
          };

          key_bindings = [
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

          mouse_bindings = [{
            mouse = "Middle";
            action = "PasteSelection";
          }];

          url = {
            launcher = "open";
            modifiers = "Shift";
          };
        }

        (mkIf (active != null) {
          import = [ "~/.config/alacritty/config/${active}.yml" ];
        })
      ]);
    };

    home.configFile = (mkIf (active != null) {
      alacritty-conf = {
        target = "alacritty/config/${active}.yml";
        text = (with config.modules.themes.font; ''
          font:
            normal:
              family: "${sans.family}"
              style:  "${sans.weight}"

            bold:
              family: "${sans.family}"
              style:  "Bold"

            italic:
              family: "${sans.family}"
              style:  "${sans.weight} Italic"

            bold_italics:
              family: "${sans.family}"
              style:  "${sans.weight} Italic"

            size: ${toString (mono.size)}

            offset:
              x: 0
              y: 0

            glyph_offset:
              x: 0
              y: 0
        '') + (with config.modules.themes.colors.main; ''
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
      };
    });
  };
}
