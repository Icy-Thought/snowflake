{ config, pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    settings = {

      # Font
      set font = "Iosevka Bold 10";

      # Base16 Gruvbox - morhetz
      set default-bg   = "#262626";
      set default-fg   = "#ebdbb2";
      
      set statusbar-fg = "#ebdbb2";
      set statusbar-bg = "#262626";
      
      set inputbar-bg  = "#262626";
      set inputbar-fg  = "#ebdbb2";
      
      set notification-error-bg  = "#262626";
      set notification-error-fg  = "#cc241d";
      
      set notification-warning-bg  = "#262626";
      set notification-warning-fg  = "#d79921";
      
      set highlight-color         = "#262626";
      set highlight-active-color  = "#ebdbb2";
      
      set completion-highlight-fg  = "#4e4e4e";
      set completion-highlight-bg  = "#87afaf";
      
      set completion-bg  = "#4e4e4e";
      set completion-fg  = "#ebdbb2";
      
      set notification-bg = "#262626";
      set notification-fg = "#458588";
      
      set recolor-lightcolor  = "#262626";
      set recolor-darkcolor   = "#ebdbb2";
      
      set recolor          = "true";
      set recolor-keephue  = "false";
      
      # General Settings
      set selection-clipboard    = "clipboard";
      set first-page-column      = "1:1";
      set adjust-open            = width;
      set window-title-basename  = true;
    };
  };

}
