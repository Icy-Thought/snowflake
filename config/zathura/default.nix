{ config, pkgs, ... }: {

  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Bold 10";
      selection-clipboard = "clipboard";
      first-page-column = "1:1";
      adjust-open = "width";
      window-title-basename = true;

      # Aquarium - FrenzyExists
      default-bg = "#1b1b23";
      default-fg = "#8791a3";

      statusbar-bg = "#1b1b23";
      statusbar-fg = "#8791a3";

      inputbar-bg = "#1b1b23";
      inputbar-fg = "#caf6bb";

      notification-bg = "#1b1b23";
      notification-fg = "#8791a3";

      notification-error-bg = "#1b1b23";
      notification-error-fg = "#8791a3";

      notification-warning-bg = "#1b1b23";
      notification-warning-fg = "#ebb9b9";

      highlight-color = "#4c5664";
      highlight-active-color = "#2c2e3e";

      completion-bg = "#2c2e3e";
      completion-fg = "#8791a3";

      completion-highlight-fg = "#ebe3b9";
      completion-highlight-bg = "#2c2e3e";

      recolor-lightcolor = "#1b1b23";
      recolor-darkcolor = "#8791a3";

      recolor = "true";
      recolor-keephue = "true";
    };

  };
}
