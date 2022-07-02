{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.dunst;
in {
  options.modules.desktop.extra.dunst = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.services.dunst = {
      enable = true;
      settings = {
        global = {
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
        };

        experimental = {
          per_monitor_dpi = false;
        };

        fullscreen_pushback_everything = {
          fullscreen = "pushback";
        };
      };
    };
  };
}
