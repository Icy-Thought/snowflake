{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) toString;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf mkMerge;

  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.alacritty = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "OpenGL terminal emulator";};

  config = mkIf config.modules.desktop.terminal.alacritty.enable {
    modules.shell.toolset.tmux.enable = true;

    hm.programs.alacritty = {
      enable = true;

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
            faux_multiplier = 3;
          };

          selection = {
            semantic_escape_chars = '',│`|:"' ()[]{}<>'';
            save_to_clipboard = false;
          };

          live_config_reload = true;

          shell = {
            program = "${getExe pkgs.fish}";
            args = ["-l" "-c" "tmux new || tmux"];
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
          import = ["~/.config/alacritty/config/${active}.yml"];
        })
      ];
    };

    home.configFile = mkIf (active != null) {
      alacritty-conf = {
        target = "alacritty/config/${active}.yml";
        text = let
          inherit (config.modules.themes.font.mono) family weight size;
          inherit (config.modules.themes.colors.main) bright normal types;
        in ''
          font:
            normal:
              family: "${family}"
              style:  "${weight}"

            italic:
              family: "${family}"
              style:  "${weight} Italic"

            bold:
              family: "${family}"
              style:  "SemiBold"

            bold_italics:
              family: "${family}"
              style:  "SemiBold Italic"

            size: ${toString size}

            offset:
              x: 0
              y: 0

            glyph_offset:
              x: 0
              y: 0

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
        '';
      };
    };
  };
}
