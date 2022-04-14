{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.terminal.kitty;
  configDir = config.snowflake.configDir;
  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.kitty = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ kitty ];

    home.configFile."kitty/kitty.conf" = {
      text = ''
        ${optionalString (active != null) ''
          include ~/.config/kitty/config/${active}.conf
        ''}

        # Xterm
        term xterm-kitty

        # General
        sync_to_monitor yes
        update_check_interval 0
        allow_remote_control no
        close_on_child_death no

        # Decorations
        repaint_delay 10
        disable_ligatures no
        inactive_text_alpha 1.0

        # Notification
        enable_audio_bell no
        bell_on_tab no
        visual_bell_duration 0.0

        # Selection + Clipboard
        strip_trailing_spaces smart
        copy_on_select clipboard
        select_by_word_characters @-./_~?&=%+#
        clipboard_control write-clipboard write-primary no-append

        # Cursor
        default_pointer_shape beam
        cursor_shape block
        cursor_blink_interval 0.5
        cursor_stop_blinking_after 15.0

        input_delay 3
        pointer_shape_when_dragging beam
        pointer_shape_when_grabbed arrow

        # Mouse
        click_interval 0.5
        mouse_hide_wait 3.0
        focus_follows_mouse yes

        # URL
        detect_urls yes
        open_url_with default
        url_prefixes http https file ftp gemini irc gopher mailto news git

        # Scrolling
        scrollback_lines 5000
        wheel_scroll_multiplier 5.0

        # Window
        remember_window_size yes
        resize_draw_strategy static

        initial_window_height 28
        initial_window_width 96

        draw_minimal_borders yes
        window_border_width 0.5pt
        window_padding_width 18.75

        # Mappings
        map ctrl+0 restore_font_size
        map ctrl+minus change_font_size all -1.0
        map ctrl+plus change_font_size all +1.0
        map ctrl+shift+delete clear_terminal reset active
        map ctrl+shift+end load_config_file
        map middle release ungrabbed paste_from_selection
      '';
    } // optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      onChange = ''
        ${pkgs.procps}/bin/pkill -USR1 -u $USER kitty || true
      '';
    };
  };
}
