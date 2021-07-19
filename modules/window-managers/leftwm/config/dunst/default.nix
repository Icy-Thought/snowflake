{ config, lib, pkgs, ... }: {

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Whitesur-dark";
      package = pkgs.whitesur-icon-theme;
      size = "16x16";
    };

    settings = {
      global = {
        ### DISPLAY ###
        monitor = 0;
        follow = "mouse";
        geometry = "300x5-30+20";
        progress_bar = true;
        progress_bar_height = 10;
        progress_bar_frame_width = 1;
        progress_bar_min_width = 150;
        progress_bar_max_width = 300;

        indicate_hidden = "yes";
        shrink = "no";
        transparency = 0;
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        text_icon_padding = 0;

        frame_width = 3;
        frame_color = "#00000080";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;

        ### TEXT ###
        font = "Inter Nerd Font 10";
        line_height = 0;
        markup = "full";
        format = ''
          <b>%s</b>
          %b'';

        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";

        ### ICONS ###
        icon_position = "left";
        min_icon_size = 0;
        max_icon_size = 32;

        ### HISTORY ###
        sticky_history = "yes";
        history_length = 20;

        ### MISC/ADVANCED ###
        browser = "firefox";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        verbosity = "mesg";
        corner_radius = 0;
        ignore_dbusclose = false;

        ### WAYLAND ###
        force_xwayland = false;

        ### LEGACY ###
        force_xinerama = false;

        ### MOUSE ###
        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "close_current";
        mouse_right_click = "close_all";

      };

      experimental = { per_monitor_dpi = false; };

      # shortcuts = {
      #   close = "ctrl+space";
      #   close_all = "ctrl+shift+space";
      #   history = "ctrl+grave";
      #   context = "ctrl+shift+period";
      # };

      urgency_low = {
        background = "#00000080";
        foreground = "#888888";
        timeout = 10;
        #icon = /path/to/icon
      };

      urgency_normal = {
        background = "#00000080";
        foreground = "#ffffff";
        timeout = 10;
        #icon = /path/to/icon
      };

      urgency_critical = {
        background = "#00000080";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
        #icon = /path/to/icon
      };
    };
  };
}
