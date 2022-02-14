{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.appliances.termEmu.alacritty;
  font = config.modules.fonts.settings;
  colors = config.modules.themes.colors;
in {
  options.modules.appliances.termEmu.alacritty = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.programs.alacritty = {
      enable = true;

      settings = {
        shell.program = "fish";
        live_config_reload = true;
        draw_bold_text_with_bright_colors = false;

        env = {
          TERM = "alacritty-direct";
          WINIT_HIDPI_FACTOR = "1";
          WINIT_X11_SCALE_FACTOR = "1";
        };

        window = {
          opacity = 1.0;
          decorations = "none";
          dynamic_title = true;
          dynamic_padding = false;

          dimensions.lines = 28;
          dimensions.columns = 96;

          position.x = 50;
          position.y = 50;

          padding.x = 25;
          padding.y = 25;
        };

        scrolling = {
          history = 5000;
          multiplier = 3;
          faux_multiplier = 3;
        };

        selection = {
          semantic_escape_chars = '',│`| = "' ()[]{}<>'';
          save_to_clipboard = false;
        };

        font = {
          size = font.size;
          use_thin_strokes = true;

          normal = {
            family = "${font.family}";
            style = "${font.style}";
          };

          bold = {
            family = "${font.family}";
            style = "Bold";
          };

          italic = {
            family = "${font.family}";
            style = "Italic";
          };

          font.offset = {
            x = 0;
            y = 0;
          };

          font.glyph_offset = {
            x = 0;
            y = 0;
          };
        };

        cursor = {
          style = "Block";
          unfocused_hollow = true;
        };

        colors.primary = {
          foreground = colors.foreground;
          background = colors.background;
        };

        colors.cursor = {
          text = colors.cursorForeground;
          cursor = colors.cursorBackground;
        };

        colors.selection = {
          text = colors.selectionForeground;
          background = colors.selectionBackground;
        };

        colors.normal = {
          black = colors.black;
          red = colors.red;
          green = colors.green;
          yellow = colors.yellow;
          blue = colors.blue;
          magenta = colors.magenta;
          cyan = colors.cyan;
          white = colors.white;
        };

        colors.bright = {
          black = colors.brightBlack;
          red = colors.brightRed;
          green = colors.brightGreen;
          yellow = colors.brightYellow;
          blue = colors.brightBlue;
          magenta = colors.brightMagenta;
          cyan = colors.brightCyan;
          white = colors.brightWhite;
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
          modifiers = "shift";
        };
      };
    };
  };
}
