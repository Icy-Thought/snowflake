{ config, pkgs, ... }: {

  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Bold 10";
      selection-clipboard = "clipboard";
      first-page-column = "1:1";
      adjust-open = "width";
      window-title-basename = true;

      # Ayu-dark -> dempfi
      default-fg = "#B3B1AD";
      default-bg = "#0A0E14";

      statusbar-fg = "#B3B1AD";
      statusbar-bg = "#0A0E14";

      inputbar-fg = "#FFB454";
      inputbar-bg = "#0A0E14";

      notification-fg = "#B3B1AD";
      notification-bg = "#0A0E14";

      notification-error-fg = "#B3B1AD";
      notification-error-bg = "#0A0E14";

      notification-warning-fg = "#F07178";
      notification-warning-bg = "#0A0E14";

      highlight-color = "#273747";
      highlight-active-color = "#1B2733";

      completion-fg = "#B3B1AD";
      completion-bg = "#1B2733";

      completion-highlight-fg = "#0A0E14";
      completion-highlight-bg = "#FFB454";

      recolor-lightcolor = "#0A0E14";
      recolor-darkcolor = "#B3B1AD";

      recolor = "true";
      recolor-keephue = "true";
    };

  };
}
