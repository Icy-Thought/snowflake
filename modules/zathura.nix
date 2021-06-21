{ config, pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      font = "Iosevka Bold 10";
      selection-clipboard = "clipboard";
      first-page-column = "1:1";
      adjust-open = "width";
      window-title-basename = true;

      # Base16 Gruvbox - morhetz
      default-bg   = "#262626";
      default-fg   = "#ebdbb2";
      
      # Status + Input bar:
      statusbar-fg = "#ebdbb2";
      statusbar-bg = "#262626";
      inputbar-bg  = "#262626";
      inputbar-fg  = "#ebdbb2";
      
      # Notification:
      notification-bg = "#262626";
      notification-fg = "#458588";
      notification-error-bg = "#262626";
      notification-error-fg = "#cc241d";
      notification-warning-bg = "#262626";
      notification-warning-fg = "#d79921";
      
      # Highlight:
      highlight-color         = "#262626";
      highlight-active-color  = "#ebdbb2";
      
      # Completion:
      completion-highlight-fg  = "#4e4e4e";
      completion-highlight-bg  = "#87afaf";
      completion-bg  = "#4e4e4e";
      completion-fg  = "#ebdbb2";
      
      # Force recolor:
      recolor-lightcolor  = "#262626";
      recolor-darkcolor   = "#ebdbb2";
      recolor          = "true";
      recolor-keephue  = "false";
    };

  };
}
