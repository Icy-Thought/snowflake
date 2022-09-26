{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.extra.dunst = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.extra.dunst.enable {
    hm.services.dunst = {
      enable = true;
      settings = with config.modules.themes; (mkMerge [
        {
          global = mkMerge [
            {
              monitor = 0;
              follow = "mouse";
              indicate_hidden = "yes";
              stack_duplicates = true;
              hide_duplicate_count = false;

              title = "Dunst";
              class = "Dunst";

              show_age_threshold = 60;
              ellipsize = "middle";
              ignore_newline = "no";
              show_indicators = "no";
              sticky_history = "no";
              history_length = 20;

              browser = "${pkgs.firefox-devedition-bin}/bin/firefox-devedition";
              always_run_script = true;
              ignore_dbusclose = false;
              force_xinerama = false;

              # Notification
              sort = "yes";
              scale = 0;
              shrink = "no";
              word_wrap = "yes";
            }
            (mkIf (active != null) {
              # Geometry
              width = 300;
              height = 200;
              origin = "top-right";
              offset = "12+48";

              padding = 20;
              horizontal_padding = 20;
              notification_limit = 0;
              separator_height = 2;

              # Progress-Bar
              progress_bar = true;
              progress_bar_height = 10;
              progress_bar_frame_width = 1;
              progress_bar_min_width = 150;
              progress_bar_max_width = 300;

              # Aesthetics
              font = "${font.sans.family} ${font.sans.weight} 11";
              frame_width = 2;
              separator_color = "frame";
              frame_color = "${colors.main.types.border}";
              transparency = 0;

              line_height = 1;
              idle_threshold = 120;
              markup = "full";
              format = "<span font='13' weight='bold'>%s</span>\\n%b";
              alignment = "left";
              vertical_alignment = "center";

              icon_position = "left";
              min_icon_size = 0;
              max_icon_size = 64;

              # Keybindings
              close = "ctrl+space";
              close_all = "ctrl+shift+space";
              history = "ctrl+grave";
              context = "ctrl+shift+period";

              mouse_left_click = "close_current";
              mouse_middle_click = "do_action, close_current";
              mouse_right_click = "close_all";
            })
          ];

          experimental = { per_monitor_dpi = false; };
          fullscreen_pushback_everything = { fullscreen = "pushback"; };
        }

        (mkIf (active != null) (with colors.main; {
          urgency_low = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            timeout = 5;
            #icon = /path/to/icon;
          };
          urgency_normal = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            frame_color = "${types.border}";
            timeout = 7;
            #icon = /path/to/icon;
          };
          urgency_critical = {
            foreground = "${types.fg}";
            background = "${types.bg}";
            frame_color = "${types.error}";
            timeout = 10;
            #icon = /path/to/icon
          };
        }))
      ]);
    };
  };
}
