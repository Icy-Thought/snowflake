{ config, ... }: {

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Whitesur-dark";
      package = pkgs.whitesur-dark;
      size = "16x16";
    };

    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        geometry = "350x5-28+58";
        indicate_hidden = "yes";
        shrink = "yes";
        transparency = 0;
        notification_height = 0;
        separator_height = 1;
        padding = 12;
        horizontal_padding = 12;
        frame_width = 1;
        frame_color = "#2c323a";
        separator_color = "#2c323a";
        sort = "yes";
        idle_threshold = 120;

        font = "JetBrains Mono Medium 10";
        line_height = 4;
        markup = "full";
        format = ''
          <span size='x-large' font_desc='JetBrains Mono 8' weight='bold' foreground='#d8dee9'>%s</span>
          %b'';

        alignment = "left";
        vertical_alignment = "top";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "end";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = true;
        show_indicators = "no";

        icon_position = "left";
        min_icon_size = 0;
        max_icon_size = 128;
        icon_path = "";

        sticky_history = "yes";
        history_length = 20;

        dmenu = "${pkgs.dmenu}/bin/dmenu" - p dunst;
        browser = "firefox";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        verbosity = "mesg";
        corner_radius = 0;
        ignore_dbusclose = false;

        force_xinerama = false;

        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "close_current";
        mouse_right_click = "close_all";

      };

      experimental = {
        per_monitor_dpi = false;
      };

      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };

      urgency_low = {
        background = "#1E2127";
        foreground = "#d8dee9";
        timeout = 5;
        #icon = /path/to/icon
      };

      urgency_normal = {
        background = "#1E2127";
        foreground = "#d8dee9";
        frame_color = "#ffcb6b";
        timeout = 7;
        #icon = /path/to/icon
      };

      urgency_critical = {
        background = "#1E2127";
        foreground = "#d8dee9";
        frame_color = "#e06c75";
        timeout = 10;
        #icon = /path/to/icon
      };

      fullscreen_pushback_everything = { fullscreen = "pushback"; };

    };
  };
}
