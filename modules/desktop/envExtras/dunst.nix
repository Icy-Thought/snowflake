{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.dunst;
in {
  options.modules.desktop.dunst.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.dunst.enable = true;
    services.dunst.iconTheme.name = "Whitesur-dark";
    services.dunst.iconTheme.package = pkgs.whitesur-icon-theme;
    services.dunst.iconTheme.size = "32x32";

    services.dunst.settings = {
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

        frame_width = 2;
        frame_color = "#FFB454";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;

        font = "JetBrainsMonoMedium Nerd Font 11";
        line_height = 0;
        markup = "full";
        format = ''
          %s %p
          %b
        '';

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

        browser = "firefox-devedition";
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
        foreground = "#E6E1CF";
        background = "#1D242C";
        timeout = 5;
        #icon = /path/to/icon
      };

      urgency_normal = {
        foreground = "#E6E1CF";
        background = "#1D242C";
        frame_color = "#FFB454";
        timeout = 7;
        #icon = /path/to/icon
      };

      urgency_critical = {
        foreground = "#E6E1CF";
        background = "#1D242C";
        frame_color = "#FF7733";
        timeout = 10;
        #icon = /path/to/icon
      };

      fullscreen_pushback_everything = { fullscreen = "pushback"; };
    };
  };
}
