{ config, lib, pkgs, ... }:

let active-palette = ./colorschemes/ayu-dark.conf;

in {
  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrainsMonoMedium Nerd Font";
      size = 13;
    };

    settings = {
      term = "xterm-kitty";
      disable_ligatures = false;
      inactive_text_alpha = "1.0";
      include = "${active-palette}";

      scrollback_lines = 5000;
      wheel_scroll_multiplier = "5.0";
      update_check_interval = 0;
      close_on_child_death = false;
      allow_remote_control = false;

      # Window-related
      remember_window_size = true;
      initial_window_height = 28;
      initial_window_width = 96;

      window_border_width = "0.5pt";
      draw_minimal_borders = true;
      window_padding_width = "18.75";

      sync_to_monitor = true;
      repaint_delay = 10;
      input_delay = 3;
      resize_draw_strategy = "static";

      # Cursor-related
      cursor_shape = "block";
      cursor_blink_interval = "0.5";
      cursor_stop_blinking_after = "15.0";
      mouse_hide_wait = "3.0";

      # Mouse-related
      click_interval = "0.5";
      focus_follows_mouse = true;
      default_pointer_shape = "beam";
      pointer_shape_when_grabbed = "arrow";
      pointer_shape_when_dragging = "beam";

      # Selection-related
      clipboard_control = "write-clipboard write-primary no-append";
      copy_on_select = "clipboard";
      strip_trailing_spaces = "smart";
      select_by_word_characters = "@-./_~?&=%+#";

      # Miscellaneous
      detect_urls = true;
      open_url_with = "default";
      url_prefixes = "http https file ftp gemini irc gopher mailto news git";

      visual_bell_duration = "0.0";
      enable_audio_bell = false;
      bell_on_tab = false;
    };

    keybindings = {
      "ctrl+shift+end" = "load_config_file";
      "ctrl+shift+delete" = "clear_terminal reset active";
      "middle" = "release ungrabbed paste_from_selection";

      # Font-related
      "ctrl+plus" = "change_font_size all +1.0";
      "ctrl+minus" = "change_font_size all -1.0";
      "ctrl+0" = "restore_font_size";
    };
  };
}
