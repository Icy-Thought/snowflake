{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.terminal.alacritty;
in {
  options.modules.desktop.terminal.alacritty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.programs.alacritty = {
      enable = true;

      settings = with config.modules.themes; (mkMerge [
        {
          env = {
            TERM = "alacritty-direct";
            WINIT_X11_SCALE_FACTOR = 1.0;
          };

          window = {
            dimensions = {
              columns = 96;
              lines = 28;
            };

            position = {
              x = 50;
              y = 50;
            };

            padding = {
              x = 25;
              y = 25;
            };

            dynamic_title = true;
            dynamic_padding = false;
            opacity = 1.0;
            decorations = "none";
          };

          scrolling = {
            history = 5000;
            multiplier = 3;
            faux_multiplier = 3;
          };

          selection = {
            semantic_escape_chars = ",│`|:\"' ()[]{}<>";
            save_to_clipboard = false;
          };

          live_config_reload = true;

          shell = {
            program = "/usr/bin/env";
            args = "- fish";
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

          mouse_bindings = [
            {
              mouse = "Middle";
              action = "PasteSelection";
            }
          ];

          url = {
            launcher = "open";
            modifiers = "Shift";
          };
        }
        (mkIf (active != null) {
          import = "~/.config/alacritty/config/${active}.yml";
        })
      ]);
    };

    home.configFile = with config.modules.themes; (mkIf (active != null) {
      "alacritty/config/${active}.yml".text =
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
        + (with config.modules.themes.colors.main; ''
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
    });
  };
}
