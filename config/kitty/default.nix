{ config, lib, pkgs, ... }: {

  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrainsMonoMedium Nerd Font";
      size = "13";
    };

    settings = {
      term = "xterm-kitty";
      font_size_delta = 1;
      disable_ligatures = false;
      inactive_text_alpha = 1.0;

      scrollback_lines = 5000;
      wheel_scroll_multiplier = 5.0;
      update_check_interval = 0;
      close_on_child_death = false;
      allow_remote_control = false;

      # Window-related
      remember_window_size = true;
      initial_window_height = 28;
      initial_window_width = 96;

      window_border_width = "0.5pt";
      draw_minimal_borders = true;
      window_margin_width = 37.5;
      window_padding_width = 18.75;

      sync_to_monitor = true;
      repaint_delay = 10;
      input_delay = 3;
      resize_draw_strategy = "static";

      # Cursor-related
      cursor_shape = "block";
      cursor_blink_interval = 0.5;
      cursor_stop_blinking_after = 15.0;
      mouse_hide_wait = 3.0;

      # Mouse-related
      click_interval = 0.5;
      focus_follows_mouse = true;
      default_pointer_shape = "beam";
      pointer_shape_when_grabbed = "arrow";
      pointer_shape_when_dragging = "beam";

      # Selection-related
      clipboard_control = "write-clipboard write-primary";
      copy_on_select = true;
      strip_trailing_spaces = "smart";
      select_by_word_characters = "@-./_~?&=%+#";

      # Miscellaneous
      detect_urls = true;
      open_url_with = "default";
      url_prefixes = "http https file ftp gemini irc gopher mailto news git";

      visual_bell_duration = 0.0;
      enable_audio_bell = false;
      bell_on_tab = false;

      # OneDark Color Palette
      # Author: Joshdick
      # Modified: Icy-Thought
      #
      cursor = "0x5c6370";
      cursor_text_color = "0x2c323c";

      foreground = "0xabb2bf";
      background = "0x1e2127";

      selection_foreground = "0x2c323c";
      selection_background = "0x3e4451";

      # Black
      color0 = "0x1e2127";
      color8 = "0x3e4452";

      # Red
      color1 = "0xe06c75";
      color9 = "0xe06c75";

      # Green
      color2 = "0x98c379";
      color10 = "0x98c379";

      # Yellow
      color3 = "0xd19a66";
      color11 = "0xd19a66";

      # Blue
      color4 = "0x61afef";
      color12 = "0x61afef";

      # Magenta
      color5 = "0xc678dd";
      color13 = "0xc678dd";

      # Cyan
      color6 = "0x56b6c2";
      color14 = "0x56b6c2";

      # White
      color7 = "0x5c6370";
      color15 = "0xabb2bf";

    };

    keybindings = {
      "ctrl+shift+delete" = "clear_terminal reset active";
      "ctrl+u" = "input_unicode_character";

      # Mouse-related
      "ctrl+left click ungrabbed" = "mouse_click_url_or_select";
      "escape" = "discard_event";
      "middle release ungrabbed" = "paste_from_selection";

      # Clipboard-related
      "ctrl+shift+c" = "copy_to_clipboard";
      "ctrl+shift+v" = "paste_from_clipboard";

      # Scroll-related
      "page_up" = "scroll_page_up";
      "page_down" = "scroll_page_down";
      "ctrl+h" = "show_scrollback";

      # Font-related
      "ctrl+equal" = "increase_font_size";
      "ctrl+minus" = "decrease_font_size";
      "ctrl+0" = "restore_font_size";
    };
  };
}
