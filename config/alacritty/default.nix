{ config, pkgs, ... }: {

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "alacritty-direct";
        WINIT_HIDPI_FACTOR = "1";
        WINIT_X11_SCALE_FACTOR = "1";
      };

      window = {
        dynamic_title = true;

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

        dynamic_padding = false;

        decorations = "none";
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

      shell = { program = "fish"; };

      live_config_reload = true;

      font = {
        normal = {
          family = "JetBrainsMonoMedium Nerd Font";
          style = "Medium";
        };

        bold = {
          family = "JetBrainsMonoMedium Nerd Font";
          style = "Bold";
        };

        italic = {
          family = "JetBrainsMonoMedium Nerd Font";
          style = "Italic";
        };

        size = 13.0;

        offset = {
          x = 0;
          y = 0;
        };

        glyph_offset = {
          x = 0;
          y = 0;
        };

        use_thin_strokes = true;
      };

      draw_bold_text_with_bright_colors = true;

      cursor = {
        style = "Block";
        unfocused_hollow = true;
      };

      background_opacity = 1.00;

      # One Dark Colorscheme
      colors = {
        # Default colors
        primary = {
          background = "#1e2127";
          foreground = "#abb2bf";
        };

        normal = {
          black = "#1e2127";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#abb2bf";
        };

        bright = {
          black = "#5c6370";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#ffffff";
        };
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

}
