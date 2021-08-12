{ config, pkgs, ... }: {

  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Bold 10";
      selection-clipboard = "clipboard";
      first-page-column = "1:1";
      adjust-open = "width";
      window-title-basename = true;

      # Base16 OneDark
      default-bg = "#1E2127";
      default-fg = "#ABB2BF";

      statusbar-bg = "#2C323C";
      statusbar-fg = "#ABB2BF";
      inputbar-bg = "#1E2127";
      inputbar-fg = "#ABB2BF";

      notification-bg = "#98C379";
      notification-fg = "#1E2127";
      notification-error-bg = "#E06C75";
      notification-error-fg = "#1E2127";
      notification-warning-bg = "#E5C07B";
      notification-warning-fg = "#1E2127";

      highlight-color = "#61AFEF";
      highlight-active-color = "#C678DD";

      completion-bg = "#2C323C";
      completion-fg = "#ABB2BF";
      completion-group-bg = "#2C323C";
      completion-group-fg = "#ABB2BF";
      completion-highlight-fg = "#2C323C";
      completion-highlight-bg = "#61AFEF";

      index-bg = "#1E2127";
      index-fg = "#ABB2BF";
      index-active-bg = "#61AFEF";
      index-active-fg = "#2C323C";

      render-loading-bg = "#2C323C";
      render-loading-fg = "#61AFEF";

      recolor-lightcolor = "#1E2127";
      recolor-darkcolor = "#ABB2BF";
      recolor = "true";
      recolor-keephue = "true";
    };

  };
}
