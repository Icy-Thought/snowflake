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
      default-bg = "#282C34";
      default-fg = "#353B45";

      statusbar-fg = "#565C64";
      statusbar-bg = "#3E4451";
      inputbar-bg = "#282C34";
      inputbar-fg = "#C8CCD4";

      notification-bg = "#282C34";
      notification-fg = "#C8CCD4";
      notification-error-bg = "#282C34";
      notification-error-fg = "#E06C75";
      notification-warning-bg = "#282C34";
      notification-warning-fg = "#E06C75";

      # Highlight:
      highlight-color = "#E5C07B";
      highlight-active-color = "#61AFEF";

      # Completion:
      completion-highlight-fg = "#353B45";
      completion-highlight-bg = "#61AFEF";
      completion-bg = "#C8CCD4";
      completion-fg = "#61AFEF";

      # Force recolor:
      recolor-lightcolor = "#282C34";
      recolor-darkcolor = "#B6BDCA";
      recolor = "true";
      recolor-keephue = "false";
    };

  };
}
