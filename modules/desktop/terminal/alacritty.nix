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
  configDir = config.snowflake.configDir;
  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.alacritty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [alacritty];

    home.configFile."alacritty/alacritty.yml".text = ''
      ${optionalString (active != null) ''
        import:
          - ~/.config/alacritty/config/${active}.yml
      ''}

      env:
        TERM: alacritty-direct
        WINIT_X11_SCALE_FACTOR: '1.0'

      window:
        dimensions:
          columns: 96
          lines: 28

        position:
          x: 50
          y: 50

        padding:
          x: 25
          y: 25

        dynamic_title: true
        dynamic_padding: false
        opacity: 1.0
        decorations: none

      scrolling:
        history: 5000
        multiplier: 3
        faux_multiplier: 3

      selection:
        semantic_escape_chars: ",│`|:\"' ()[]{}<>"
        save_to_clipboard: false

      live_config_reload: true

      shell:
        program: /usr/bin/env
        args:
          - fish

      cursor:
        style: Block
        unfocused_hollow: true

      key_bindings:
        - { key: N,              mods: Control|Shift, action: SpawnNewInstance }
        - { key: Q,              mods: Control,       action: Quit             }
        - { key: V,              mods: Control|Shift, action: Paste            }
        - { key: C,              mods: Control|Shift, action: Copy             }
        - { key: NumpadAdd,      mods: Control,       action: IncreaseFontSize }
        - { key: NumpadSubtract, mods: Control,       action: DecreaseFontSize }
        - { key: Key0,           mods: Control,       action: ResetFontSize    }

      mouse_bindings:
        - { mouse: Middle, action: PasteSelection }

      url:
        launcher: open
        modifiers: Shift
    '';
  };
}
