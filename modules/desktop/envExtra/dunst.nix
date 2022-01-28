{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.envExtra.dunst;
  font = config.modules.themes.font;
  fontStyle = config.modules.themes.fontStyle;
in {
  options.modules.desktop.envExtra.dunst = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.services.dunst = {
      enable = true;
      iconTheme.name = "Whitesur-dark";
      iconTheme.package = pkgs.whitesur-icon-theme;
      iconTheme.size = "32x32";

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
          padding = 20;
          horizontal_padding = 20;

          frame_width = 2;
          frame_color = "#FF977A";
          separator_color = "frame";
          sort = "yes";
          idle_threshold = 120;

          font = "${font} 10";
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
          foreground = "#e6e1cf";
          background = "#1d242c";
          timeout = 5;
          #icon = /path/to/icon
        };

        urgency_normal = {
          foreground = "#e6e1cf";
          background = "#1d242c";
          frame_color = "#ff977a";
          timeout = 7;
          #icon = /path/to/icon
        };

        urgency_critical = {
          foreground = "#e6e1cf";
          background = "#1d242c";
          frame_color = "#ff7733";
          timeout = 10;
          #icon = /path/to/icon
        };

        fullscreen_pushback_everything = { fullscreen = "pushback"; };
      };
    };
  };
}
