{ config, lib, pkgs, ... }: {

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Whitesur-dark";
      package = pkgs.whitesur-icon-theme;
      size = "32x32";
    };

    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        geometry = "320x100-12+48";
        indicate_hidden = "yes";
        shrink = "no";
        transparency = 0;
        notification_height = 0;
        separator_height = 2;
        corner_radius = 5;
        padding = 20;
        horizontal_padding = 20;
        frame_width = 1;
        frame_color = "#F3F8FE";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;

        font = "JetBrainsMonoMedium Nerd Font 11";
        line_height = 0;
        markup = "full";
        format = "%s %p\n%b";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "no";

        icon_position = "left";
        min_icon_size = 0;
        max_icon_size = 64;
        # icon_path = "";
        sticky_history = "no";
        history_length = 20;

        browser = "firefox";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        verbosity = "mesg";
        ignore_dbusclose = false;
        force_xinerama = false;

        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";

      };

      experimental = { per_monitor_dpi = false; };

      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };

      urgency_low = {
        background = "#1E2127";
        foreground = "#F3F8FE";
        timeout = 5;
        #icon = /path/to/icon
      };

      urgency_normal = {
        background = "#1E2127";
        foreground = "#F3F8FE";
        frame_color = "#0096FF";
        timeout = 7;
        #icon = /path/to/icon
      };

      urgency_critical = {
        background = "#1E2127";
        foreground = "#F3F8FE";
        frame_color = "#E06C75";
        timeout = 10;
        #icon = /path/to/icon
      };

      fullscreen_pushback_everything = { fullscreen = "pushback"; };

    };
  };
}
