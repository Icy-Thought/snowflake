{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.terminal.kitty;
  configDir = config.snowflake.configDir;
  active = config.modules.themes.active;
in {
  options.modules.desktop.terminal.kitty = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [kitty];

    home.configFile = {
      "kitty/tab_bar.py".source = "${configDir}/kitty/tab_bar.py";

      "kitty/kitty.conf".text = ''
        ${optionalString (active != null) ''
          include ~/.config/kitty/config/${active}.conf
        ''}

        # }----------=[ XTerm ]=---------- {
        term xterm-kitty

        # }----------=[ General ]=---------- {
        sync_to_monitor         yes
        update_check_interval   0
        allow_remote_control    no
        close_on_child_death    no
        shell_integration       no-cursor
        confirm_os_window_close -1

        # }----------=[ Decorations ]=---------- {
        repaint_delay       10
        disable_ligatures   cursor
        adjust_line_height  113%
        inactive_text_alpha 1.0

        # }----------=[ Notification ]=---------- {
        enable_audio_bell    no
        bell_on_tab          no
        visual_bell_duration 0.0

        # }----------=[ Selection + Clip ]=---------- {
        strip_trailing_spaces     smart
        copy_on_select            clipboard
        select_by_word_characters @-./_~?&=%+#
        clipboard_control         write-clipboard write-primary no-append

        # }----------=[ Cursor ]=---------- {
        default_pointer_shape      beam
        cursor_shape               block
        cursor_blink_interval      0.5
        cursor_stop_blinking_after 15.0

        input_delay 3
        pointer_shape_when_dragging beam
        pointer_shape_when_grabbed  arrow

        # }----------=[ Mouse ]=---------- {
        click_interval      0.5
        mouse_hide_wait     3.0
        focus_follows_mouse yes

        # }----------=[ URL ]=---------- {
        detect_urls   yes
        open_url_with default
        url_prefixes  http https file ftp gemini irc gopher mailto news git

        # }----------=[ Scrolling ]=---------- {
        scrollback_lines        5000
        wheel_scroll_multiplier 5.0

        # }----------=[ Windows ]=---------- {
        initial_window_height 28
        initial_window_width  96
        remember_window_size  yes
        resize_draw_strategy  static

        window_border_width  1.0
        window_margin_width  0.0
        window_padding_width 15.00
        placement_strategy   top-left
        draw_minimal_borders yes

        # }----------=[ Tabs ]=---------- {
        tab_bar_style           custom
        tab_separator           ""
        tab_fade                0 0 0 0
        tab_activity_symbol     none
        tab_bar_edge            top
        tab_bar_margin_height   0.0 0.0
        active_tab_font_style   bold-italic
        inactive_tab_font_style normal
        tab_bar_min_tabs        1
        bell_on_tab             no

        # }----------=[ Mappings ]=---------- {
        map ctrl+shift+end           load_config_file
        map ctrl+shift+0             restore_font_size
        map middle release ungrabbed paste_from_selection

        map ctrl+shift+t new_tab_with_cwd
        map ctrl+shift+j next_tab
        map ctrl+shift+k previous_tab
      '';
    };
  };
}
