{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.termEmu.alacritty;
in {
  options.modules.desktop.termEmu.alacritty = {
    enable = mkBoolOpt false;
    activePalette = mkOpt (nullOr str) null;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.alacritty.enable = true;
      programs.alacritty.settings = {
        env.TERM = "alacritty-direct";
        env.WINIT_HIDPI_FACTOR = "1";
        env.WINIT_X11_SCALE_FACTOR = "1";

        window.dynamic_title = true;
        window.dimensions.columns = 96;
        window.dimensions.lines = 28;

        window.position.x = 50;
        window.position.y = 50;

        window.padding.x = 25;
        window.padding.y = 25;

        window.dynamic_padding = false;
        window.decorations = "none";

        scrolling.history = 5000;
        scrolling.multiplier = 3;
        scrolling.faux_multiplier = 3;

        selection.semantic_escape_chars = '',│`|:"' ()[]{}<>'';
        selection.save_to_clipboard = false;

        shell.program = "fish";

        live_config_reload = true;

        font.normal.family = "JetBrainsMonoMedium Nerd Font";
        font.normal.style = "Medium";

        font.bold.family = "JetBrainsMonoMedium Nerd Font";
        font.bold.style = "Bold";

        font.italic.family = "JetBrainsMonoMedium Nerd Font";
        font.italic.style = "Italic";

        font.size = 13.0;

        font.offset.x = 0;
        font.offset.y = 0;

        font.glyph_offset.x = 0;
        font.glyph_offset.y = 0;

        font.use_thin_strokes = true;

        draw_bold_text_with_bright_colors = false;

        cursor.style = "Block";
        cursor.unfocused_hollow = true;

        background_opacity = 1.0;

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

        url.launcher = "open";
        url.modifiers = "shift";
      };
    }

    (mkIf cfg.activePalette == "ayu-dark" {
      programs.kitty.settings.import =
        [ "${configDir}/alacritty/ayu-dark.yaml" ];
    })

    (mkIf cfg.activePalette == "onedark" {
      programs.kitty.settings.import =
        [ "${configDir}/alacritty/onedark.yaml" ];
    })
  ]);
}
