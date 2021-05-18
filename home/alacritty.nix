{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {

      env = {
        "TERM" = "alacritty-direct";
      };

      window = {
        dimensions = {
          lines = 29;
          columns = 118;
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

      cursor = {
        style = "Block";
        unfocused_hollow = true;
      };

      selection = {
        semantic_escape_chars = ",│`|:\"' ()[]{}<>";
        save_to_clipboard = false;
      };

      shell = {
        program = "fish";
      };

      live_config_reload = true;

      font = {
        normal.family = "Iosevka SemiBold";
        bold.family   = "Iosevka SemiBold";
        italic.family = "Iosevka SemiBold";

        size = 12.0;
        use_thin_strokes = true;

        offset = {
          x = 0;
          y = 0;
        };

        glyph_offset = {
          x = 0;
          y = 0;
        };
      };

      draw_bold_text_with_bright_colors = true;

      background_opacity = 0.95;

      # Colors (One Dark)
      colors = {
        # Default colors
        primary = {
          background = "0x1e2127";
          foreground = "0xabb2bf";
        };
      
        # Normal colors
        normal = {
          black      = "0x1e2127";
          red        = "0xe06c75";
          green      = "0x98c379";
          yellow     = "0xd19a66";
          blue       = "0x61afef";
          magenta    = "0xc678dd";
          cyan       = "0x56b6c2";
          white      = "0xabb2bf";
        };
      
        # Bright colors
        bright = {
          black      =  "0x5c6370";
          red        =  "0xe06c75";
          green      =  "0x98c379";
          yellow     =  "0xd19a66";
          blue       =  "0x61afef";
          magenta    =  "0xc678dd";
          cyan       =  "0x56b6c2";
          white      =  "0xffffff";
        };
      };

      keybindings = [
        { key = "N";                mods = "Control|Shift";   actions = "SpawnNewInstance"; }
        { key = "Q";                mods = "Control";         action  = "Quit";             }
        { key = "V";                mods = "Control|Shift";   actions = "Paste";            }
        { key = "C";                mods = "Control|Shift";   actions = "Copy";             }
        { key = "NumpaddAdd";       mods = "Control";         actions = "IncreaseFontSize"; }
        { key = "NumpadSubtract";   mods = "Control";         actions = "DecreaseFontSize"; }
        { key = "Key0";             mods = "Control";         actions = "ResetFontSize";    }
      ];

      mouse_bindings = [
        { mouse = "Middle";         actions = "PasteSelection"; }
      ];

      url = {
        launcher  = "open";
        modifiers = "shift";
      };

    };
  };

}
